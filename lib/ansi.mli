(** An ANSI escape sequence parser. *)

type t

val create : unit -> t
(** Create a new parser in the default state. *)

val process : t -> string -> string
(** [process t data] reads in [data] and generates HTML output, parsing escape sequences as it goes.
    If [data] ends with a partial sequence, it remembers this and will resume processing on the
    next call to [process]. *)

val css : string
(** Some default CSS rules to make the HTML output appear in colour. *)

val css_solarized : string
(** Some default CSS rules to make the HTML output appear in colour, with the Solarized colorscheme
    (light variant).
    @see https://ethanschoonover.com/solarized/ *)

val css_solarized_dark : string
(** Some default CSS rules to make the HTML output appear in colour, with the Solarized colorscheme
    (dark variant).
    @see https://ethanschoonover.com/solarized/ *)

val strip : string -> string
(** [strip data] reads in [data] and strips out ANSI sequences. It doesn't remember partial
    sequences between calls to [strip]. *)
