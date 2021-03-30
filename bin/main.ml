open Js_of_ocaml
open Lib
module Html = Dom_html

let js = Js.string

let document = Html.document

let state = Types.new_state 910 666 7 128 64

let main () =
  Gui.reset_canvas state;
  Gui.setup_canvas state;
  let rec loop () =
    Gui.draw_gui state;
    Html.window##requestAnimationFrame
      (Js.wrap_callback (fun _ -> loop ()))
    |> ignore
  in
  loop ()

let _ = main ()
