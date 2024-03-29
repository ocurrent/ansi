type base_colour = [ `Black | `Blue | `Cyan | `Green | `Magenta | `Red | `White | `Yellow ]
type colour = [ base_colour | `Default | `Rgb of int | `Hi of base_colour ]

type sgr =
  [ `BgCol of colour
  | `Bold
  | `Faint
  | `FgCol of colour
  | `Italic
  | `NormalIntensity
  | `NoItalic
  | `NoReverse
  | `NoUnderline
  | `Reset
  | `Reverse
  | `Underline
  | `DoubleUnderline ]

type escape =
  [ `Reset
  | `ST
  | `Ctrl of [ `SelectGraphicRendition of sgr list ]
  | `OSC of [ `Hyperlink of ((string * string) list * string) option ]
  ]

val parse :
  Char_stream.t ->
  [ `Literal of Char_stream.t
  | `Escape of escape * Char_stream.t
  | `Invalid of Char_stream.t
  | `Incomplete ]
(** [parse stream] returns the first token in [stream] and the stream directly after it,
    or [`Incomplete] if more data is required to parse the first token.
    [`Literal s2] indicates that everything between [stream] and [s2] should be output as literal text.
    [`Escape (e, s2)] indicates that the first token was escape sequence [e].
    [`Invalid s2] indicates that the first token was malformed or not understood and processing should continue
    from [s2].
*)

val strip : string -> string
