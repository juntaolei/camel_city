open Js_of_ocaml
open Lib
module Html = Dom_html

(** Initial game configuration *)
let state = State.new_state 1200 750 9 128 64

(** [main ()] is the game loop that redraws the GUI based on updates to
    the game state. *)
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

(** Program entrypoint *)
let _ = main ()
