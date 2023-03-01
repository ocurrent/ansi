type style = [ `Normal | `Faint ]

let style_to_code : style -> int = function `Normal -> 00 | `Faint -> 02

let set_foreground_color ch ~styles c =
  List.iter (fun style -> Printf.fprintf ch "\x1b[%d;%dm" (style_to_code style) c) styles

let set_background_color ch ~styles c =
  List.iter (fun style -> Printf.fprintf ch "\x1b[%d;%dm" (style_to_code style) c) styles

let reset_output ch =
  output_string ch "\x1b[0m"

let output_string ch str =
  output_string ch str;
  reset_output ch

let output ?(styles=[`Normal]) ?(fg="\u{2588}") ?(bg=" ") ch =
  for i = 30 to 37 do
    set_foreground_color ch ~styles i;
    output_string ch fg
  done;
  for i = 90 to 97 do
    set_foreground_color ch ~styles i;
    output_string ch fg
  done;
  output_char ch '\n';
  for i = 40 to 47 do
    set_background_color ch ~styles i;
    output_string ch bg
  done;
  for i = 100 to 107 do
    set_background_color ch ~styles i;
    output_string ch bg
  done;
  output_char ch '\n';
  ()

let () =
  output ~styles:[`Normal] stdout;
  output ~styles:[`Faint] stdout;
  ()
