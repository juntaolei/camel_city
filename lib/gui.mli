(** The Gui module provides the entrypoint to the game GUI. Inside the
    module, there are various functions defined for the GUI to function
    properly. However, not all functions are exposed as the user of this
    module should only need to trigger the entire GUI logic through
    [main]. *)

(** [main] is the only exposed entrypoint to the game. The function
    starts up the JavaScript related GUI functionalities for the game. *)
val main : unit
