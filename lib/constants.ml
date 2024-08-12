open Tsdl

let trans_r, trans_g, trans_b = (255, 0, 255)

let screen_resolution = (1275, 720)

let screen_tile_resolution = (25, 15)

let tiles_per_row = fst screen_tile_resolution

let tiles_per_column = snd screen_tile_resolution

let tile_w, tile_h =
  ( fst screen_resolution / tiles_per_row
  , snd screen_resolution / tiles_per_column )

let kW = Sdl.K.w

and kA = Sdl.K.a

and kS = Sdl.K.s

and kD = Sdl.K.d

and kEsc = Sdl.K.escape

and kP = Sdl.K.p

and kM = Sdl.K.m
