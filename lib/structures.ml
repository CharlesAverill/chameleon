open Map
open Atlas

let load_structure name sprite_atlas =
  let tilename_of_int n =
    (List.find (fun sprite -> sprite.idx = n) sprite_atlas).filename
  in
  let name = Filename.concat "assets/structures/" name ^ ".csv" in
  let file_contents =
    let lines = ref [] in
    let file = open_in name in
    try
      while true do
        lines := input_line file :: !lines
      done ;
      !lines
    with End_of_file -> close_in file ; List.rev !lines
  in
  Array.init (List.length file_contents) (fun i ->
      let line = List.nth file_contents i in
      Array.of_list
        (List.map tile_of_string
           (List.map tilename_of_int
              (List.map int_of_string (String.split_on_char ',' line)) ) ) )

let update_structure (map : map) tl_x tl_y (structure : tile array array) =
  snd
    (Array.fold_left
       (fun (row_number, map_top) row ->
         ( row_number + 1
         , snd
             (Array.fold_left
                (fun (col_number, map_bot) tile ->
                  ( col_number + 1
                  , update_map map_bot (tl_x + col_number) (tl_y + row_number)
                      tile ) )
                (0, map_top) row ) ) )
       (0, map) structure )

let house_map atlas =
  update_structure
    (fun _ _ -> Color Black)
    0 0
    (load_structure "house_interior" atlas)
