(* Inspired of https://github.com/JohnMorales/dotfiles/blob/master/colors/24-bit-color.sh *)

(** This file echoes four gradients with 24-bit color codes
   to the terminal to demonstrate their functionality.
   The foreground escape sequence is [^[38;2;<r>;<g>;<b>m]
   The background escape sequence is [^[48;2;<r>;<g>;<b>m]
   [<r> <g> <b>] range from 0 to 255 inclusive.
   The escape sequence [^[0m] returns output to default
 *)

let set_foreground_color ch ~sep r g b =
  Printf.fprintf ch "\x1b[38%c2%c%d%c%d%c%dm" sep sep r sep g sep b

let set_background_color ch ~sep r g b =
  Printf.fprintf ch "\x1b[48%c2%c%d%c%d%c%dm" sep sep r sep g sep b

let reset_output ch =
  output_string ch "\x1b[0m"

let output_string ch str =
  output_string ch str;
  reset_output ch

(** Gives a color [c/255 %] along HSV
  # Who knows what happens when [c] is outside 0-255
  # Returns [red, green, blue] where
  # [red] [green] and [blue] are integers
  # ranging between 0 and 255 inclusive  *)
let rainbow_color c =
  let h = c / 43 in
  let f = c - 43 * h in
  let t = f * 255 / 43 in
  let q = 255 - t in

  match h with
  | 0 -> 255, t, 0
  | 1 -> q, 255, 0
  | 2 -> 0, 255, t
  | 3 -> 0, q, 255
  | 4 -> t, 0, 255
  | 5 -> 255, 0, q
  | _ -> assert false

let output ch str f =
  for i = 0 to 127 do
    f i 0 0;
    output_string ch str
  done;
  output_char ch '\n';
  for i = 255 downto 128 do
    f i 0 0;
    output_string ch str
  done;
  output_char ch '\n';

  for i = 0 to 127 do
    f 0 i 0;
    output_string ch str
  done;
  output_char ch '\n';
  for i = 255 downto 128 do
    f 0 i 0;
    output_string ch str
  done;
  output_char ch '\n';

  for i = 0 to 127 do
    f 0 0 i;
    output_string ch str
  done;
  output_char ch '\n';
  for i = 255 downto 128 do
    f 0 0 i;
    output_string ch str
  done;
  output_char ch '\n';

  for i = 0 to 127 do
    let r, g, b = rainbow_color i in
    f r g b;
    output_string ch str
  done;
  output_char ch '\n';
  for i = 255 downto 128 do
    let r, g, b = rainbow_color i in
    f r g b;
    output_string ch str
  done;
  output_char ch '\n';
  ()

let output ch =
  output ch "\u{2588}" @@ set_foreground_color ch ~sep:':';
  output ch "\u{2588}" @@ set_foreground_color ch ~sep:';';
  output ch " " @@ set_background_color ch ~sep:':';
  output ch " " @@ set_background_color ch ~sep:';'

let () = output stdout
