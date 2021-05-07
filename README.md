# OCurrent ANSI escape parser

Program output (such as build logs) often includes ANSI escape codes to colour and style the output.
These codes are intended for terminals and are distracting when displayed as text e.g. in web pages.
This library inteprets some of the common codes and can convert them to HTML, producing basic styled output
(e.g. highlighting errors in red).

It is intended to be used by [OCurrent][], but may be useful in other projects too.

## Licensing

The OCurrent ANSI parser is licensed under the Apache License, Version 2.0.
See [LICENSE][] for the full license text.

[OCurrent]: https://github.com/ocurrent/ocurrent
[LICENSE]: ./LICENSE
