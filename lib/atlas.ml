(* Code for interfacing with a sprite atlas *)

open Tsdl
open Logging
open Constants

type atlas_image =
  {filename: string; idx: int; rect: Sdl.rect; texture: Sdl.texture}

let get_sprite fn =
  try List.find (fun ai -> ai.filename = fn)
  with Not_found ->
    fatal rc_Error ("Failed to find sprite " ^ fn ^ " in atlas")

let make_transparent surf =
  handle
    (Sdl.set_color_key surf false
       (Sdl.map_rgb
          (handle (Sdl.alloc_format (Sdl.get_surface_format_enum surf)))
          0 0 0 ) ) ;
  handle
    (Sdl.set_color_key surf true
       (Sdl.map_rgb
          (handle (Sdl.alloc_format (Sdl.get_surface_format_enum surf)))
          trans_r trans_g trans_b ) )

let init_atlas assets_path renderer =
  let data =
    ( Marshal.from_channel (open_in (Filename.concat assets_path "atlas.dat"))
      : (string * int * int * int * int * int * bool) list )
  and bmp = handle (Sdl.load_bmp (Filename.concat assets_path "atlas.bmp")) in
  make_transparent bmp ;
  let texture = handle (Sdl.create_texture_from_surface renderer bmp) in
  let data =
    List.fold_left
      (fun acc (fn, i, x, y, w, h, _rotated) ->
        {filename= fn; idx= i; rect= Sdl.Rect.create ~x ~y ~w ~h; texture}
        :: acc )
      [] data
  in
  Sdl.free_surface bmp ; data
