(* (based on https://github.com/moby/datakit/blob/master/ci/src/cI_web.ml) *)

open Astring

let max_escape_length = 20

type gfx_state = {
  bold : bool;
  italic : bool;
  underline : bool;
  fg : Escape_parser.colour;
  bg : Escape_parser.colour;
  reversed : bool;
  links : string list;
}

type t = {
  mutable gfx_state : gfx_state;
  mutable buf : string;
}

let default_gfx_state = {
  bold = false; italic = false; underline = false;
  fg = `Default; bg = `Default;
  reversed = false;
  links = [];
}

let name_of_colour : Escape_parser.base_colour -> string = function
  | `Black -> "black"
  | `Blue -> "blue"
  | `Cyan -> "cyan"
  | `Green -> "green"
  | `Magenta -> "magenta"
  | `Red -> "red"
  | `White -> "white"
  | `Yellow -> "yellow"

let name_of_colour : Escape_parser.colour -> string option = function
  | `Default | `Rgb _ -> None
  | `Hi colour -> Some (name_of_colour colour)
  | (#Escape_parser.base_colour as colour) -> Some (name_of_colour colour)

let is_bright : Escape_parser.colour -> bool = function `Hi _ -> true | _ -> false

let apply_ctrl state : Escape_parser.sgr -> gfx_state = function
  | `Bold -> { state with bold = true }
  | `NoBold -> { state with bold = false }
  | `FgCol fg -> { state with fg }
  | `BgCol bg -> { state with bg }
  | `Reverse -> { state with reversed = true }
  | `NoReverse -> { state with reversed = false }
  | `Italic -> { state with italic = true }
  | `NoItalic -> { state with italic = false }
  | `Underline -> { state with underline = true }
  | `NoUnderline -> { state with underline = false }
  | `Reset -> default_gfx_state

let apply_osc state = function
  | Some (_params, link) -> { state with links = link :: state.links}
  | None -> (match state.links with _ :: links -> { state with links } | [] -> state)

let pp_attr attr ~sep f = function
  | [] -> ()
  | cls -> Fmt.(pf f " %s='%a'" attr (list ~sep string) cls)
let pp_class = pp_attr "class" ~sep:Fmt.(const string " ")
let pp_style = pp_attr "style" ~sep:Fmt.(const string "; ")

let with_style s txt =
  match s with
  | s when s = default_gfx_state -> txt
  | { bold; italic; underline; fg; bg; reversed; links } ->
      let bg, fg = if reversed then fg, bg else bg, fg in
      let cl ty bright = function
        | Some c -> [ Printf.sprintf "%s-%s%s" ty (if bright then "bright-" else "") c ]
        | None -> []
      in
      let cls = if bold then [ "bold" ] else [] in
      let cls = if italic then "italic" :: cls else cls in
      let cls = if underline then "underline" :: cls else cls in
      let cls = cl "fg" (is_bright fg) (name_of_colour fg) @ cls in
      let cls = cl "bg" (is_bright bg) (name_of_colour bg) @ cls in
      let style = function
        | (`Rgb x, `Fg) -> [ Printf.sprintf "color: #%06x" x ]
        | (`Rgb x, `Bg) -> [ Printf.sprintf "background-color: #%06x" x ]
        | _ -> []
      in
      let style = style (fg, `Fg) @ style (bg, `Bg) in
      match links with
      | [] -> Fmt.str "<span%a%a>%s</span>" pp_class cls pp_style style txt
      | link :: _ ->
         Fmt.str "<span%a%a><a href='%s'>%s</a></span>" pp_class cls pp_style style link txt
      (* | _ -> failwith "unimplemented" *)

let create () = { gfx_state = default_gfx_state; buf = "" }

let process t data =
  let output = Buffer.create (String.length data * 2) in
  let add = Buffer.add_string output in
  let module Stream = Char_stream in
  let write (s, first, stop) =
    let data = String.with_range s ~first ~len:(stop - first) in
    add (Xml_print.encode_unsafe_char data |> with_style t.gfx_state)
  in
  let rec aux s =
    match Escape_parser.parse s with
    | `Literal i when Stream.equal i s -> `Done ""
    | `Literal i ->
        write Stream.(s -- i);
        aux i
    | `Incomplete when Stream.avail s >= max_escape_length ->
        add "<b>ESCAPE-TOO-LONG</b>";
        aux (Stream.skip s)
    | `Incomplete -> `Done (Stream.to_string s)
    | `Invalid i -> aux i
    | `Escape (`Reset, i) ->
        t.gfx_state <- default_gfx_state;
        aux i
    | `Escape (`Ctrl (`SelectGraphicRendition c), i) ->
        t.gfx_state <- List.fold_left apply_ctrl t.gfx_state c;
        aux i
    | `Escape (`OSC (`Hyperlink l), i) ->
       t.gfx_state <- apply_osc t.gfx_state l;
       aux i
    | `Escape (`ST, i) ->
       aux i
  in
  let (`Done unprocessed) = aux (Stream.of_string (t.buf ^ data)) in
  t.buf <- unprocessed;
  Buffer.contents output

let css = Style.css

let css_dark = Style.css_dark

let css_solarized = Style.css_solarized

let css_solarized_dark = Style.css_solarized_dark

let strip = Escape_parser.strip
