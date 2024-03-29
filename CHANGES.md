### 0.7.0 (2023-09-18)

* Fix CSS for italic. (#18, @haochenx)

* Respect reverse attr when default color is active. (#17, @haochenx)

* Support faint and double underline styles. (#14, @MisterDA)

### 0.6.0 (2022-12-20)

* Support OSC hyperlinks. (#13, @MisterDA)
  https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda

* Support italic and underline escape sequences with `.italic` and
  `.underline` CSS classes. (#11, @MisterDA)

* Fix range of intermediate bytes. (#8, @MisterDA)

* Add `Ansi.strip` to strip ANSI escape sequences part of a string.
  Support stripping OSC sequences.
  (#7, #9, #12, @MisterDA)

* Support 8-bit colour palette, bright colours. (#6, @MisterDA)

* Expose default foreground and background colors with `.fg-default`
  and `.bg-default` CSS classes. (#6, #10, MisterDA)

* Fix bold/bright confusion, expose `.bold` CSS class. (#6, #10 @MisterDA)

* Expose alternative 16-colours colorschemes (xterm dark, solarized
  light and dark). (#6, @MisterDA)

### 0.5.0 (2022-03-31)

* Rename to `ansi` (#2, @samoht)
* ansi: 24-bit colour parsing (#1, @copy)
* Split into its own repository.

### v0.3 and v0.4

No changes (released as part of OCurrent).

### v0.2

Make transitive dependencies explicit in dune file.

### v0.1

Initial release.
