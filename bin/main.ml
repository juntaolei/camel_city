open Js_of_ocaml
open Lib
module Html = Dom_html

let js = Js.string

let document = Html.document

let state = Types.new_state 910 666 7 128 64

let rec loop _ =
  Gui.draw_gui state;
  Html.window##requestAnimationFrame
    (Js.wrap_callback (fun _ -> loop ()))
  |> ignore

let _ = loop ()
