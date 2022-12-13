let set_foreground_color ch ~sep c =
  Printf.fprintf ch "\x1b[38%c5%c%dm" sep sep c

let set_background_color ch ~sep c =
  Printf.fprintf ch "\x1b[48%c5%c%dm" sep sep c

let reset_output ch =
  output_string ch "\x1b[0m"

let output_string ch str =
  output_string ch str;
  reset_output ch

let output ch str f =
  for i = 0 to 7 do
    f i;
    output_string ch str
  done;
  for i = 8 to 15 do
    f i;
    output_string ch str
  done;
  output_char ch '\n';

  for i = 16 to 231 do
    f i;
    output_string ch str;
    if i mod 35 = 15 + i / 35 then (
      reset_output ch;
      output_char ch '\n'
    )
  done;

  for i = 232 to 255 do
    f i;
    output_string ch str
  done;
  output_char ch '\n';
  ()

let output ch =
  output ch "\u{2588}" @@ set_foreground_color ch ~sep:':';
  output ch "\u{2588}" @@ set_foreground_color ch ~sep:';';
  output ch " " @@ set_background_color ch ~sep:':';
  output ch " " @@ set_background_color ch ~sep:';';
  ()

let () = output stdout
