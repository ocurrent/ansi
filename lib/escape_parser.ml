module Stream = Char_stream

type base_colour = [ `Black | `Blue | `Cyan | `Green | `Magenta | `Red | `White | `Yellow ]
type colour = [ base_colour | `Default | `Rgb of int | `Hi of base_colour ]

type sgr =
  [ `BgCol of colour
  | `Bold
  | `FgCol of colour
  | `Italic
  | `NoBold
  | `NoItalic
  | `NoReverse
  | `NoUnderline
  | `Reset
  | `Reverse
  | `Underline ]

type escape =
  [ `Reset
  | `ST
  | `Ctrl of [ `SelectGraphicRendition of sgr list ]
  | `OSC of [ `Hyperlink of ((string * string) list * string) option ]
  ]

let is_param_byte c =
  let c = Char.code c in
  c land 0xf0 = 0x30

let is_im_byte c =
  let c = Char.code c in
  c land 0xf0 = 0x20

let is_final_byte c =
  let c = Char.code c in
  c >= 0x40 && c <= 0x7e

let is_escape_sequence_byte c =
  let c = Char.code c in
  c >= 0x20 && c <= 0x7e

exception Unknown_escape

let colour = function
  | 0 -> `Black
  | 1 -> `Red
  | 2 -> `Green
  | 3 -> `Yellow
  | 4 -> `Blue
  | 5 -> `Magenta
  | 6 -> `Cyan
  | 7 -> `White
  | _ -> raise Unknown_escape

let sgr = function
  | 0 -> `Reset
  | 1 -> `Bold
  | 3 -> `Italic
  | 4 -> `Underline
  | 7 -> `Reverse
  | 22 -> `NoBold
  | 23 -> `NoItalic
  | 24 -> `NoUnderline
  | 27 -> `NoReverse
  | x when x >= 30 && x <= 37 -> `FgCol (colour (x - 30))
  | x when x >= 90 && x <= 97 -> `FgCol (`Hi (colour (x - 90))) (* Non-standard "bright" fg colour *)
  | 39 -> `FgCol `Default
  | x when x >= 40 && x <= 47 -> `BgCol (colour (x - 40))
  | x when x >= 100 && x <= 107 -> `BgCol (`Hi (colour (x - 100))) (* Non-standard "bright" bg colour *)
  | 49 -> `BgCol `Default
  | _ -> raise Unknown_escape

let rgb r g b = `Rgb (r lsl 16 lor g lsl 8 lor b)

let colour256 n =
  if 0 <= n && n <= 7 then
    colour n
  else if 8 <= n && n <= 15 then
    `Hi (colour (n - 8))
  else if 16 <= n && n <= 231 then
    let cube y =
      let i = ((n - 16) / y) mod 6 in
      if i = 0 then 0 else (14135 + 10280 * i) / 256
    in
    let r = cube 36 and g = cube 6 and b = cube 1 in
    rgb r g b
  else if 232 <= n && n <= 255 then
    let x = (n - 232) * 10 + 0x08 in
    rgb x x x
  else
    raise Unknown_escape

let sgrs params =
  match params with
  | "" :: _ -> [ `Reset ]
  | _ ->
    match List.map int_of_string params with
    | exception Failure _ ->
      raise Unknown_escape
    | params ->
      let rec go = function
        | 38 :: 5 :: n :: rest ->
          `FgCol (colour256 n) :: go rest
        | 48 :: 5 :: n :: rest ->
          `BgCol (colour256 n) :: go rest
        | 38 :: 2 :: r :: g :: b :: rest ->
          `FgCol (rgb r g b) :: go rest
        | 48 :: 2 :: r :: g :: b :: rest ->
          `BgCol (rgb r g b) :: go rest
        | n :: rest ->
          sgr n :: go rest
        | [] -> []
      in
      go params

let parse_ctrl ~params = function
  | "m" -> `SelectGraphicRendition (sgrs params)
  | _ -> raise Unknown_escape

let read_intermediates ~params start =
  let rec aux s =
    match Stream.next s with
    | None -> `Incomplete (* No final byte *)
    | Some (x, s) when is_im_byte x -> aux s
    | Some (x, s2) when is_final_byte x -> (
        let func = Stream.(start -- s2 |> string_of_span) in
        let params = Astring.String.cuts ~sep:";" params in
        let params = List.concat_map (Astring.String.cuts ~sep:":") params in
        try `Escape (`Ctrl (parse_ctrl ~params func), s2)
        with Unknown_escape -> `Invalid s2 )
    | Some _ -> `Invalid s
  in
  aux start

let read_params start =
  let rec aux s =
    match Stream.next s with
    | None -> `Incomplete (* No final byte *)
    | Some (x, s) when is_param_byte x -> aux s
    | Some _ ->
        let params = Stream.(start -- s |> string_of_span) in
        read_intermediates ~params s
  in
  aux start

let is_param_osc c =
  let c = Char.code c in
  (c >= 0x08 && c <= 0x0d) || (c >= 0x20 && c <= 0x7e)

let read_intermediates_osc ~params s =
  match params with
  | ["8"; ""; ""] -> `Escape (`OSC (`Hyperlink None), s)
  | ["8"; ""; link] -> `Escape (`OSC (`Hyperlink (Some ([], link))), s)
  | ["8"; params; link] -> (
    try
      let params =
        (* not the prettiest *)
        Astring.String.cuts ~sep:":" params
        |> List.map (fun param ->
               match Astring.String.cut ~sep:"=" param with
               | Some param -> param
               | None -> raise Unknown_escape)
      in
      `Escape (`OSC (`Hyperlink (Some (params, link))), s)
    with Unknown_escape -> `Invalid s)
  | _ -> `Invalid s

let read_params_osc start =
  let rec aux s =
    match Stream.next s with
    | None -> `Incomplete (* No final byte *)
    | Some (x, s) when is_param_osc x ->
       aux s
    | Some _ ->
       let params =
         Stream.(start -- s |> string_of_span)
         |> Astring.String.cuts ~sep:";" in
       read_intermediates_osc ~params s
  in
  aux start

(* Parse [esc], an escape sequence. *)
let parse_escape esc =
  match Stream.(next (Stream.skip esc)) with
  | Some ('[', s) -> read_params s (* [esc] is a control sequence *)
  | Some (']', s) -> read_params_osc s (* [esc] is a operating system command sequence *)
  | Some ('c', s) -> `Escape (`Reset, s)
  | Some ('\\', s) -> `Escape (`ST, s)
  | Some (_, s) -> `Invalid s (* TODO: other types of escape *)
  | None -> `Incomplete

let parse input =
  (* In theory, we could also get the 8-bit escape character encoded as two
     UTF-8 bytes, but for now we just process the "<ESC>[" sequence, which
     seems to be what everyone is using. *)
  match Stream.find input '\x1b' with
  | None -> `Literal (Stream.skip_all input)
  | Some i when Stream.equal input i -> parse_escape input
  | Some i -> `Literal i

let strip str =
  let len = String.length str in
  let buf = Buffer.create len in
  let rec loop start i =
    if i = len then (
      if i - start > 0 then Buffer.add_substring buf str start (i - start);
      Buffer.contents buf)
    else
      match String.unsafe_get str i with
      | '\x1b' ->
         if i - start > 0 then Buffer.add_substring buf str start (i - start);
         skip_parse_escape (i + 1)
      | _ -> loop start (i + 1)
  and skip_parse_escape i =
    if i = len then Buffer.contents buf
    else
      match String.unsafe_get str i with
      | ']' -> skip_osc i
      | c when is_escape_sequence_byte c -> skip (i + 1)
      | _ -> loop (i - 1) (i + 1)
  and skip i =
    if i = len then Buffer.contents buf
    else
      match String.unsafe_get str i with
      | c when is_final_byte c -> loop (i + 1) (i + 1)
      | _ -> skip (i + 1)
  and skip_osc i =
    if i = len then Buffer.contents buf
    else
      match String.unsafe_get str i with
      | '\x1b' ->
         let i = i + 1 in
         if i = len then Buffer.contents buf
         else
           (match String.unsafe_get str i with
            | '\\' -> loop (i + 1) (i + 1)
            | _ -> skip_osc (i + 1))
      | _ -> skip_osc (i + 1)
  in
  loop 0 0
