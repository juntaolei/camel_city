open Js_of_ocaml
open Lib
module Html = Dom_html

let js = Js.string

let document = Html.document

let state = State.new_state 1200 750 9 128 64

let main () =
  Gui.reset_gui state;
  Gui.setup_gui state;
  Gui.add_event_listeners state;
  let rec loop () =
    Gui.draw_gui state;
    Html.window##requestAnimationFrame
      (Js.wrap_callback (fun _ -> loop ()))
    |> ignore
  in
  loop ()

let _ = main ()
