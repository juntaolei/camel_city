open Js_of_ocaml
module Html = Dom_html

let js = Js.string

let document = Html.window##.document

let onload _ =
  let gui =
    Js.Opt.get
      (document##getElementById (js "gui"))
      (fun () -> assert false)
  in
  Dom.appendChild gui (Html.createCanvas document);
  Js._false

let _ = Html.window##.onload := Html.handler onload
