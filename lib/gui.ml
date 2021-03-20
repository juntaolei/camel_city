open Js_of_ocaml
open Types

let draw_cell (ctx : Dom_html.canvasRenderingContext2D Js.t) x y config
    =
  let width = cell_width config in
  let height = cell_height config in
  let fill_style = cell_fill_style config in
  ctx##beginPath;
  ctx##moveTo (x -. (width /. 2.)) y;
  ctx##lineTo (x -. width) (y +. (height /. 2.));
  ctx##lineTo (x -. (width /. 2.)) (y +. height);
  ctx##lineTo x (y +. (height /. 2.));
  ctx##lineTo (x -. (width /. 2.)) y;
  ctx##stroke;
  ctx##.fillStyle := Js.string fill_style;
  ctx##fill

let draw_map (ctx : Dom_html.canvasRenderingContext2D Js.t) config =
  for i = 0 to map_width config do
    for j = 0 to map_height config do
      let i' = float_of_int i in
      let j' = float_of_int j in
      let x =
        ((i' -. j') *. cell_width config /. 2.)
        +. (canvas_width config /. 2.)
      in
      let y =
        ((i' +. j') *. cell_height config /. 2.) +. cell_height config
      in
      draw_cell ctx x y config
    done
  done

let reset_canvas (ctx : Dom_html.canvasRenderingContext2D Js.t) config =
  let width = canvas_width config in
  let height = canvas_height config in
  ctx##clearRect 0. 0. width height
