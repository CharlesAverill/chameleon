open Tsdl
open Chameleon.Atlas
open Chameleon.Logging
open Chameleon.Constants
open Chameleon.Map
open Chameleon.Structures

let last_tile_was_teleporter = ref false

type map_stack = (map * (int * int) ref) list ref

let push stack i = stack := i :: !stack

let pop stack =
  match !stack with
  | [] ->
      fatal rc_Error "Tried to pop from empty stack"
  | h :: t ->
      stack := t ;
      h

let peek stack =
  match !stack with
  | [] ->
      fatal rc_Error "Tried to peek from empty stack"
  | h :: _ ->
      h

let print_map (map_stack : map_stack) =
  let map, player_pos = peek map_stack in
  for row = 0 to tiles_per_column do
    for col = 0 to tiles_per_row do
      let tile_x, tile_y =
        ( fst !player_pos + col - (tiles_per_row / 2)
        , snd !player_pos + row - (tiles_per_column / 2) )
      in
      Printf.printf "%s" (letters_of_tile (map tile_x tile_y))
    done ;
    print_newline ()
  done

let print_info player_pos =
  Printf.printf "Pos: (%d, %d)\n" (fst !player_pos) (snd !player_pos) ;
  flush stdout

let add_pos (dx, dy) (x, y) = (dx + x, dy + y)

let move_pos move (map_stack : map_stack) creatures =
  let map, player_pos = peek map_stack in
  last_tile_was_teleporter :=
    teleporter (map (fst !player_pos) (snd !player_pos)) ;
  let nx, ny = add_pos !player_pos move in
  let can_move = collision (map nx ny) && !creatures nx ny = None in
  if can_move then player_pos := (nx, ny)

let rec event_loop (map_stack : map_stack) creatures =
  let event = Sdl.Event.create () in
  while Sdl.poll_event (Some event) do
    handle_event event map_stack creatures
  done

and handle_event e map_stack creatures =
  match Sdl.Event.(enum @@ get e typ) with
  | `Quit ->
      exit 0 (* Exit the program on window close *)
  | `Key_down ->
      let keysym = Sdl.Event.get e Sdl.Event.keyboard_keycode in
      handle_keydown_event keysym map_stack creatures
  | _ ->
      ()

and handle_keydown_event (keysym : int) (map_stack : map_stack) creatures =
  let _, player_pos = peek map_stack in
  match keysym with
  | x when x = kS ->
      move_pos (0, 1) map_stack creatures
  | x when x = kA ->
      move_pos (-1, 0) map_stack creatures
  | x when x = kW ->
      move_pos (0, -1) map_stack creatures
  | x when x = kD ->
      move_pos (1, 0) map_stack creatures
  | x when x = kEsc ->
      exit 0
  | x when x = kP ->
      print_info player_pos
  | x when x = kM ->
      print_map map_stack
  | _ ->
      ()

let get_map_tile (map_stack : map_stack) x y =
  let map, _ = peek map_stack in
  map x y
(* if (x, y) = !player_pos then Player else map x y *)

let draw_tile sprite x y renderer =
  handle
    (Sdl.render_copy ~src:sprite.rect
       ~dst:
         (Sdl.Rect.create ~x:(x * tile_w) ~y:(y * tile_h) ~w:tile_w ~h:tile_h)
       renderer sprite.texture )

let draw_tiles (map_stack : map_stack) renderer sprite_atlas creatures =
  let _, player_pos = peek map_stack in
  for row = 0 to tiles_per_column do
    for col = 0 to tiles_per_row do
      let tile_x, tile_y =
        ( fst !player_pos + col - (tiles_per_row / 2)
        , snd !player_pos + row - (tiles_per_column / 2) )
      in
      let tile_sprite =
        get_sprite
          (string_of_tile (get_map_tile map_stack tile_x tile_y))
          sprite_atlas
      in
      draw_tile tile_sprite col row renderer ;
      ( match !creatures tile_x tile_y with
      | None ->
          ()
      | Some s ->
          draw_tile
            (get_sprite ("creatures/" ^ s) sprite_atlas)
            col row renderer ) ;
      if (tile_x, tile_y) = !player_pos then
        let tile_sprite = get_sprite "smiley" sprite_atlas in
        draw_tile tile_sprite col row renderer
    done
  done

let update_game map_stack atlas =
  let map, player_pos = peek map_stack in
  let reset_steps = ref true in
  ( if not !last_tile_was_teleporter then
      match map (fst !player_pos) (snd !player_pos) with
      | Village ->
          push map_stack (house_map atlas, ref (6, 7))
      | Door ->
          let _, _ = pop map_stack in
          ()
      | _ ->
          reset_steps := false ) ;
  if !reset_steps then last_tile_was_teleporter := true

let main () =
  (* Initialize SDL *)
  handle (Sdl.init Sdl.Init.(video + events)) ;
  let _, renderer =
    handle
      (Sdl.create_window_and_renderer ~w:(fst screen_resolution)
         ~h:(snd screen_resolution) Sdl.Window.opengl )
  in
  (* Load sprites *)
  let sprite_atlas = init_atlas "assets" renderer in
  (* Set background color *)
  handle (Sdl.set_render_draw_color renderer 0 0 0 0) ;
  (* Setup frame limit *)
  let target_fps = 60 in
  let frame_time = Int32.of_int (1000 / target_fps) in
  let current_time = ref (Sdl.get_ticks ()) in
  let acc : int32 ref = ref Int32.zero in
  let elapsed = ref Int32.zero in
  (* Initialize map *)
  let map = proc_gen () in
  let map_stack = ref [(map, ref (0, 0))] in
  let creatures =
    ref (fun x y -> if (x, y) = (1, -1) then Some "cat" else None)
  in
  (* Game loop *)
  while true do
    let now = Sdl.get_ticks () in
    let delta = Int32.sub now !current_time in
    current_time := now ;
    (* Handle input *)
    event_loop map_stack creatures ;
    acc := Int32.add !acc delta ;
    while Int32.compare !acc frame_time >= 0 do
      (* Update game *)
      update_game map_stack sprite_atlas ;
      (* update map; *)
      acc := Int32.sub !acc frame_time
    done ;
    (* Render game *)
    handle (Sdl.render_clear renderer) ;
    draw_tiles map_stack renderer sprite_atlas creatures ;
    Sdl.render_present renderer ;
    (* Cap FPS *)
    let frame_real_duration = Int32.sub (Sdl.get_ticks ()) now in
    if Int32.compare frame_real_duration frame_time < 0 then
      Sdl.delay (Int32.sub frame_time frame_real_duration) ;
    elapsed := Int32.add !elapsed frame_time ;
    if Int32.compare !elapsed Int32.one >= 0 then
      (* Printf.printf "FPS: %ld\n"
         (Int32.div 1000l (if delta = 0l then 1l else delta)) ; *)
      (* flush stdout ; *)
      elapsed := Int32.zero
  done

let () = main ()
