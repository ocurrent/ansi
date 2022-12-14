let set_foreground_color ch c =
  Printf.fprintf ch "\x1b[%dm" c

let set_background_color ch c =
  Printf.fprintf ch "\x1b[%dm" c

let reset_output ch =
  output_string ch "\x1b[0m"

let output_string ch str =
  output_string ch str;
  reset_output ch

let output ch =
  for i = 30 to 37 do
    set_foreground_color ch i;
    output_string ch "\u{2588}"
  done;
  for i = 90 to 97 do
    set_foreground_color ch i;
    output_string ch "\u{2588}"
  done;
  output_char ch '\n';
  for i = 40 to 47 do
    set_background_color ch i;
    output_string ch " "
  done;
  for i = 100 to 107 do
    set_background_color ch i;
    output_string ch " "
  done;
  output_char ch '\n';
  ()

let () = output stdout
