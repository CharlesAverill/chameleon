(* Procedural generation *)

open Noise
open Logging

type wave = {seed: int; frequency: float; amplitude: float}

let int_map f n =
  let rec range = function 0 -> [0] | x -> x :: range (x - 1) in
  List.map f (List.rev (range n))

let seed = 43

let scale = 40.

let ground = 0.8

let forest = 0.85

let desert = 0.35

let cold = 0.35

let deep = 0.45

let hot = 0.65

let land' = 1.05

let village = 0.8

type height = Low | High

type color = Blue | Black

and tile =
  | Water of height
  | Grass
  | Stone
  | Sand
  | Forest
  | Snow
  | Village
  | Player
  | Carpet of color
  | Door
  | Wood
  | Color of color

let collision = function Water Low | Stone | Wood -> false | _ -> true

let teleporter = function Door | Village -> true | _ -> false

let string_of_color = function Blue -> "blue" | Black -> "black"

let color_of_string = function
  | "blue" ->
      Blue
  | "black" ->
      Black
  | _ ->
      failwith "Unknown color"

let get_ocean height temp _humid =
  Water (if height +. (temp /. 2.) < deep then Low else High)

let get_temperate_land height temp _humid =
  if height < ground then if temp < cold then Snow else Grass else Stone

let get_hot_land height _temp humid =
  if height < ground then
    if humid < desert then Sand else if humid > forest then Forest else Grass
  else Stone

let get_land height temp humid structures =
  if structures > village then Village
  else if temp < hot then get_temperate_land height temp humid
  else get_hot_land height temp humid

let get_tile height temp humid structures landness =
  if landness +. height < land' then get_ocean height temp humid
  else get_land height temp humid structures

let string_of_tile = function
  | Water Low ->
      "water/deep"
  | Water High ->
      "water/shallow"
  | Grass ->
      "grass"
  | Stone ->
      "cobblestone"
  | Sand ->
      "sand"
  | Forest ->
      "forest"
  | Snow ->
      "snow"
  | Village ->
      "village"
  | Player ->
      "smiley"
  | Carpet c ->
      Printf.sprintf "building/carpet_%s" (string_of_color c)
  | Door ->
      "building/door"
  | Wood ->
      "building/wood"
  | Color c ->
      Printf.sprintf "colors/%s" (string_of_color c)

let tile_of_string s =
  match s with
  | "water/deep" ->
      Water Low
  | "water/shallow" ->
      Water High
  | "grass" ->
      Grass
  | "cobblestone" ->
      Stone
  | "forest" ->
      Forest
  | "snow" ->
      Snow
  | "village" ->
      Village
  | "smiley" ->
      Player
  | s when String.starts_with ~prefix:"carpet_" s ->
      let carpet_color = List.nth (String.split_on_char '_' s) 1 in
      Carpet (color_of_string carpet_color)
  | "building/door" ->
      Door
  | "building/wood" ->
      Wood
  | "building/carpet_blue" ->
      Carpet Blue
  | _ ->
      fatal rc_Error ("Unknown tile " ^ s)

let letters_of_tile t =
  let max = 2 in
  let rep =
    match t with
    | Water Low ->
        "WL"
    | Water High ->
        "WH"
    | Grass ->
        "G"
    | Stone ->
        "C"
    | Sand ->
        "S"
    | Forest ->
        "F"
    | Snow ->
        "SN"
    | Village ->
        "V"
    | Player ->
        "=)"
    | Carpet _ ->
        "CT"
    | Door ->
        "D"
    | Wood ->
        "W"
    | Color _ ->
        "CL"
  in
  String.cat rep (String.make (max - String.length rep) ' ')

let remove (q : 'a Queue.t) (i : 'a) =
  let q' = Queue.fold (fun acc x -> if x = i then acc else x :: acc) [] q in
  Queue.clear q ;
  Queue.add_seq q (List.to_seq q')

type map = int -> int -> tile

let proc_gen_raw () x y =
  let x, y = (float_of_int x, float_of_int y) in
  let height = perlin2d x y scale seed in
  let temperature = perlin2d x y (scale *. 15.) (seed + 1) in
  let humidity = perlin2d x y (scale *. 15.) (seed + 2) in
  let structures = perlin2d x y (scale *. 5.) (seed + 3) in
  let landness = perlin2d x y (scale *. 500.) (seed + 4) in
  let tile = get_tile height temperature humidity structures landness in
  tile

let proc_gen : unit -> map =
  let tile_cache_size = 1000 in
  let tile_cache = Hashtbl.create tile_cache_size in
  let tile_order = Queue.create () in
  let evict_oldest_tile () =
    if Queue.length tile_order >= tile_cache_size then
      let oldest_tile = Queue.pop tile_order in
      Hashtbl.remove tile_cache oldest_tile
  in
  fun x y ->
    let cache_key = (x, y) in
    match Hashtbl.find_opt tile_cache cache_key with
    | Some cached_tile ->
        (* Move the accessed tile to the front of the queue *)
        (* remove tile_order cache_key ;
           Queue.push cache_key tile_order ; *)
        cached_tile
    | None ->
        let new_tile = proc_gen_raw x y in
        Hashtbl.add tile_cache cache_key new_tile ;
        Queue.push cache_key tile_order ;
        evict_oldest_tile () ;
        new_tile

let update_map (map : map) x y tile nx ny =
  if (x, y) = (nx, ny) then tile else map nx ny
