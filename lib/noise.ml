open Ctypes
open Foreign

let snoise3 = Dl.dlopen ~flags:[Dl.RTLD_LAZY] ~filename:"lib/snoise3.so"

let easy_noise3 =
  foreign ~from:snoise3 "easy_noise3"
    (float @-> float @-> float @-> float @-> int @-> returning float)

(* This crashes with "corrupted size vs. prev_size" and I'm not sure why
   The C code runs fine on its own
*)
let easy_noise2_radius x y scale seed radius =
  let xi, yi = (int_of_float x, int_of_float y) in
  List.mapi
    (fun i v -> ((xi + (i / radius), yi + (i mod radius)), v))
    (CArray.to_list
       (Ctypes.CArray.from_ptr
          ((foreign ~from:snoise3 "easy_noise2_radius"
              ( float @-> float @-> float @-> int @-> int
              @-> returning (ptr float) ) )
             x y scale seed radius )
          (radius * radius) ) )

let perlin2d x y scale seed = easy_noise3 x y 0. scale seed
