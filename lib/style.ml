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

let pp_rgb fmt (r, g, b) =
  Fmt.pf fmt "rgb(%d, %d, %d)" r g b

let pp_escape fmt dof bright name rgb =
  Fmt.pf fmt "pre span.%s-%s%s { %s: %a }\n"
    (match dof with `Fg -> "fg" | `Bg -> "bg")
    (if bright then "bright-" else "")
    name
    (match dof with `Fg -> "color" | `Bg -> "background")
    pp_rgb
    rgb

let pp_colors fmt dof bright t =
  pp_escape fmt dof bright "black" t.black;
  pp_escape fmt dof bright "red" t.red;
  pp_escape fmt dof bright "green" t.green;
  pp_escape fmt dof bright "yellow" t.yellow;
  pp_escape fmt dof bright "blue" t.blue;
  pp_escape fmt dof bright "magenta" t.magenta;
  pp_escape fmt dof bright "cyan" t.cyan;
  pp_escape fmt dof bright "white" t.white

let pp fmt (colors, bright_colors) =
  pp_colors fmt `Fg false colors;
  pp_colors fmt `Fg true bright_colors;
  pp_colors fmt `Bg false colors;
  pp_colors fmt `Bg true bright_colors

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

let css = Fmt.str "%a" pp (default, default_bright)
