let pp_params =
  let buf = Buffer.create 64 in
  let rec aux = function
    | [] ->
       let str = Buffer.contents buf in
       Buffer.clear buf;
       str
    | [key, value] ->
       Buffer.add_string buf key;
       Buffer.add_char buf '=';
       Buffer.add_string buf value;
       aux []
    | (key, value) :: tl ->
       Buffer.add_string buf key;
       Buffer.add_char buf '=';
       Buffer.add_string buf value;
       Buffer.add_char buf ':';
       aux tl
  in
  function None -> "" | Some params -> aux params

let link' ~params ~link ~text =
  Printf.sprintf "\x1b]8;%s;%s\x1b\\%s\x1b]8;;\x1b\\" (pp_params params) link text

let link ch ~params ~link ~text =
  output_string ch (link' ~params ~link ~text)

let set_foreground_color' c =
  Printf.sprintf "\x1b[%dm" c

let set_foreground_color ch c =
  output_string ch (set_foreground_color' c)

let reset_output' = "\x1b[0m"

let reset_output ch =
  output_string ch reset_output'

let () =
  let params = None in
  let ch = stdout in
  link ch ~params ~link:"https://example.com" ~text:"This is a link";
  output_char ch '\n';
  link ch ~params ~link:"https://ocurrent.org" ~text:"OCurrent website";
  output_char ch '\n';
  set_foreground_color ch 31;
  link ch ~params ~link:"https://red-carpet.com" ~text:"Red foregroung";
  reset_output ch;
  output_char ch '\n';

  let inner = link' ~params ~link:"https://inner-website.org" ~text:"inner" in
  link ch ~params ~link:"https://outer-website.com" ~text:(Printf.sprintf "outer1 %s outer2" inner);
  output_char ch '\n';

  let inner =
    (set_foreground_color' 32)
    ^ "inner green"
    ^ reset_output'
  in
  let inner = link' ~params ~link:"https://inner-website.org" ~text:inner in
  link ch ~params ~link:"https://outer-website.com" ~text:(Printf.sprintf "outer1 %s outer2" inner);
  output_char ch '\n';
  (* This is also how alacritty 0.11.0 (8dbaa0bb) renders the outer-inner-green. *)

  ()
