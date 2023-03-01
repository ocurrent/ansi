type rgb = int * int * int

type t =
  {
    black : rgb;
    red : rgb;
    green : rgb;
    yellow : rgb;
    blue : rgb;
    magenta : rgb;
    cyan : rgb;
    white : rgb;
  }

(* Depth of Field colors *)
type dof =
  {
    fg : rgb;
    bg : rgb;
  }

let faint_c c = int_of_float (float_of_int c *. 2. /. 3.)

let pp_rgb fmt ~faint (r, g, b) =
  let r, g, b = if faint then faint_c r, faint_c g, faint_c b else r, g, b in
  Fmt.pf fmt "rgb(%d, %d, %d)" r g b

let pp_escape fmt dof ~bright ~faint name rgb =
  Fmt.pf fmt "pre span.%s-%s%s%s { %s: %a }\n"
    (match dof with `Fg -> "fg" | `Bg -> "bg")
    (if bright then "bright-" else "")
    name
    (if faint then ".faint" else "")
    (match dof with `Fg -> "color" | `Bg -> "background")
    (pp_rgb ~faint)
    rgb

let pp_colors fmt dof ~bright ~faint t =
  pp_escape fmt dof ~bright ~faint "black" t.black;
  pp_escape fmt dof ~bright ~faint "red" t.red;
  pp_escape fmt dof ~bright ~faint "green" t.green;
  pp_escape fmt dof ~bright ~faint "yellow" t.yellow;
  pp_escape fmt dof ~bright ~faint "blue" t.blue;
  pp_escape fmt dof ~bright ~faint "magenta" t.magenta;
  pp_escape fmt dof ~bright ~faint "cyan" t.cyan;
  pp_escape fmt dof ~bright ~faint "white" t.white

let pp_dof fmt dof rgb =
  Fmt.pf fmt ".%s-default { %s: %a }\n"
    (match dof with `Fg -> "fg" | `Bg -> "bg")
    (match dof with `Fg -> "color" | `Bg -> "background")
    (pp_rgb ~faint:false)
    rgb

let pp fmt (colors, bright_colors, dof) =
  pp_colors fmt `Fg ~bright:false ~faint:false colors;
  pp_colors fmt `Fg ~bright:false ~faint:true colors;
  pp_colors fmt `Fg ~bright:true ~faint:false bright_colors;
  pp_colors fmt `Fg ~bright:true ~faint:true bright_colors;
  pp_colors fmt `Bg ~bright:false ~faint:false colors;
  pp_colors fmt `Bg ~bright:true ~faint:false bright_colors;
  pp_dof fmt `Fg dof.fg;
  pp_dof fmt `Bg dof.bg;
  Fmt.pf fmt
{|pre span.bold { font-weight: bold }
pre span.italic { font-weight: italic }
pre span.underline { text-decoration: underline }
pre span.double-underline { text-decoration: underline double; }
|}

let default =
  {
    black = 0, 0, 0;
    red = 205, 0, 0;
    green = 0, 205, 0;
    yellow = 205, 205, 0;
    blue = 0, 0, 238;
    magenta = 205, 0, 205;
    cyan = 0, 205, 205;
    white = 229, 229, 229;
  }

let default_bright =
  {
    black = 127, 127, 127;
    red = 255, 0, 0;
    green = 0, 255, 0;
    yellow = 255, 255, 0;
    blue = 92, 92, 255;
    magenta = 255, 0, 255;
    cyan = 0, 255, 255;
    white = 255, 255, 255;
  }

let default_dof =
  {
    fg = default.black;
    bg = default_bright.white;
  }

let css = Fmt.str "%a" pp (default, default_bright, default_dof)

let default_dark =
  {
    default with
    black = default.white;
    white = default.black;
  }

let default_dark_bright =
  {
    default_bright with
    black = default_bright.white;
    white = default_bright.black;
  }

let default_dark_dof =
  {
    fg = default_dof.bg;
    bg = default_dof.fg;
  }

let css_dark = Fmt.str "%a" pp (default_dark, default_dark_bright, default_dark_dof)

let solarized =
  {
    black = 7, 54, 66;
    red = 220, 50, 47;
    green = 133, 153, 0;
    yellow = 181, 137, 0;
    blue = 39, 139, 210;
    magenta = 211, 54, 130;
    cyan = 42, 161, 152;
    white = 238, 232, 213;
  }

let solarized_bright =
  {
    black = 0, 43, 54;
    red = 203, 75, 22;
    green = 88, 110, 117;
    yellow = 101, 123, 131;
    blue = 131, 148, 150;
    magenta = 108, 113, 196;
    cyan = 147, 161, 161;
    white = 253, 246, 227;
  }

let solarized_dof =
  {
    fg = solarized.black;
    bg = solarized.white;
  }

let solarized_dark =
  {
    solarized with
    black = solarized.white;
    white = solarized.black;
  }

let solarized_dark_bright =
  {
    solarized_bright with
    black = solarized_bright.white;
    green = solarized_bright.cyan;
    yellow = solarized_bright.blue;
    blue = solarized_bright.yellow;
    cyan = solarized_bright.green;
    white = solarized_bright.black;
  }

let solarized_dark_dof =
  {
    fg = solarized.white;
    bg = solarized.black;
  }

let css_solarized =
  Fmt.str "%a" pp (solarized, solarized_bright, solarized_dof)
let css_solarized_dark =
  Fmt.str "%a" pp (solarized_dark, solarized_dark_bright, solarized_dark_dof)
