(* Adapted from https://www.parallelrealities.co.uk/tutorials/atlas/atlas1.php *)

open Tsdl
open Chameleon.Logging
open Chameleon.Constants

let get_ok x =
  let _ = Result.get_ok x in
  ()

type image = {fn: string; surface: Sdl.surface}

let dir_contents dir =
  let rec loop result = function
    | f :: fs when Sys.is_directory f ->
        Sys.readdir f |> Array.to_list
        |> List.map (Filename.concat f)
        |> List.append fs |> loop result
    | f :: fs ->
        loop (f :: result) fs
    | [] ->
        result
  in
  loop [] [dir]

let make_transparent surf =
  handle
    (Sdl.set_color_key surf true
       (Sdl.map_rgb
          (handle (Sdl.alloc_format (Sdl.get_surface_format_enum surf)))
          trans_r trans_g trans_b ) )

let init_images dir =
  let a =
    Array.of_list
      (List.map
         (fun s ->
           let surface = handle ~rc:rc_FileError (Sdl.load_bmp s) in
           get_ok (Sdl.set_surface_blend_mode surface Sdl.Blend.mode_none) ;
           make_transparent surface ;
           {fn= s; surface} )
         (dir_contents dir) )
  in
  Array.sort
    (fun i1 i2 ->
      snd (Sdl.get_surface_size i1.surface)
      - snd (Sdl.get_surface_size i2.surface) )
    a ;
  a

type node =
  {x: int; y: int; w: int; h: int; children: (node ref * node ref) option}

let rec _string_of_node ?(indent = 0) node =
  let istr = "\n" ^ String.make (indent * 2) ' ' in
  (if indent <> 0 then istr else "")
  ^ String.concat istr
      [ "----------"
      ; Printf.sprintf "X: %d" node.x
      ; Printf.sprintf "Y: %d" node.y
      ; Printf.sprintf "W: %d" node.w
      ; Printf.sprintf "H: %d" node.h
      ; Printf.sprintf "Children: %s"
          ( match node.children with
          | None ->
              "None"
          | Some (c1, c2) ->
              _string_of_node ~indent:(indent + 1) !c1
              ^ _string_of_node ~indent:(indent + 1) !c2 ) ]

let padding = 0

let split_node node w h =
  { node with
    children=
      Some
        ( ref
            { x= node.x + w + padding
            ; y= node.y
            ; w= node.w - w - padding
            ; h
            ; children= None }
        , ref
            { x= node.x
            ; y= node.y + h + padding
            ; w= node.w
            ; h= node.h - h - padding
            ; children= None } ) }

let rec find_node root' w h =
  let root = !root' in
  match root.children with
  | None ->
      if w <= root.w && h <= root.h then (
        root' := split_node root w h ;
        Some !root' )
      else None
  | Some (l, r) -> (
    match find_node l w h with None -> find_node r w h | Some x -> Some x )

let get_pixel surface x y =
  let bpp =
    Sdl.get_pixel_format_bytes_pp
      (handle (Sdl.alloc_format (Sdl.get_surface_format_enum surface)))
  in
  handle (Sdl.lock_surface surface) ;
  let p =
    Bigarray.Array1.get
      (Sdl.get_surface_pixels surface Bigarray.int32)
      ((y * Sdl.get_surface_pitch surface) + (x * bpp))
  in
  Sdl.unlock_surface surface ; p

let set_pixel surface x y p =
  let bpp =
    Sdl.get_pixel_format_bytes_pp
      (handle (Sdl.alloc_format (Sdl.get_surface_format_enum surface)))
  in
  (Sdl.get_surface_pixels surface Bigarray.Int32).{y
                                                   * Sdl.get_surface_pitch
                                                       surface
                                                   + (x * bpp)} <- p

let blit_rotated (src : Sdl.surface) (dest : Sdl.surface) dest_x dest_y =
  let sw, sh = Sdl.get_surface_size src in
  let dy = ref 0 in
  for x = 0 to sw - 1 do
    let dx = ref (sh - 1) in
    for y = 0 to sh - 1 do
      set_pixel dest (dest_x + !dx) (dest_y + !dy) (get_pixel src x y) ;
      dx := !dx - 1
    done ;
    dy := !dy + 1
  done

let remove_prefix prefix str =
  let prefix_len = String.length prefix in
  if String.length str >= prefix_len && String.sub str 0 prefix_len = prefix
  then String.sub str prefix_len (String.length str - prefix_len)
  else str

let main () =
  get_ok (Sdl.init Sdl.Init.video) ;
  let dir = "assets/tiles" in
  let images = init_images dir in
  let n_images = Array.length images in
  let atlas_size = 256 in
  let root = ref {x= 0; y= 0; w= atlas_size; h= atlas_size; children= None}
  and atlas =
    handle
      (Sdl.create_rgb_surface ~w:atlas_size ~h:atlas_size ~depth:32 0xffl
         0xff00l 0xff0000l 0xff000000l )
  in
  let n = ref (Some !root) in
  let rotations = ref 0 in
  let to_marshal = ref [] in
  for i = 0 to n_images - 1 do
    let rotated = ref false in
    let w, h = Sdl.get_surface_size images.(i).surface in
    n := find_node root h w ;
    (* print_endline (string_of_node (Option.get !n)) ; *)
    if !n = None then (
      rotated := true ;
      n := find_node root w h ) ;
    match !n with
    | Some x ->
        if !rotated then (
          rotations := !rotations + 1 ;
          n := Some {x with w= x.h; h= x.w} ) ;
        let dest = Sdl.Rect.create ~x:x.x ~y:x.y ~w:x.w ~h:x.h in
        to_marshal :=
          ( Filename.chop_extension (remove_prefix "assets/tiles/" images.(i).fn)
          , i
          , Sdl.Rect.x dest
          , Sdl.Rect.y dest
          , w
          , h
          , !rotated )
          :: !to_marshal ;
        if not !rotated then
          handle
            (Sdl.blit_surface ~src:images.(i).surface None ~dst:atlas (Some dest))
        else
          blit_rotated images.(i).surface atlas (Sdl.Rect.x dest)
            (Sdl.Rect.y dest) ;
        Printf.printf "[%02d/%02d] %s\n" (i + 1) n_images images.(i).fn
    | None ->
        _log Log_Error ("Couldn't add " ^ images.(i).fn) ;
        Sdl.free_surface images.(i).surface
  done ;
  (* Save atlas image file *)
  handle (Sdl.save_bmp atlas (Filename.concat dir "../atlas.bmp")) ;
  (* Save atlas metadata *)
  Marshal.to_channel
    (open_out (Filename.concat dir "../atlas.dat"))
    !to_marshal []

let _ = main ()
