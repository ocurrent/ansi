let test_strip () =
  let actual =
    (String.concat "\n"
       (List.map Ansi.strip [
            "\x1b[34mthe lazy fox\x1b[39m jumps over the brown dog\x1b[0m";
            "the lazy fox \x1b[34mjumps over\x1b[39m the brown dog\x1b[0m";
            "\x1b[34mthe lazy fox\x1b[m\x1b[39m jumps over \x1b[mthe brown dog"
    ]));
  and expected =
    {|the lazy fox jumps over the brown dog
the lazy fox jumps over the brown dog
the lazy fox jumps over the brown dog|}
  in
  Alcotest.(check string) "strip escape sequences" expected actual;
  let actual =
    "\x1b]8;;http://example.com\x1b\\This is a link\x1b]8;;\x1b\\"
    |> Ansi.strip
  and expected = "This is a link"
  in
  Alcotest.(check string) "strip hyperlink (OSC)" expected actual;
  ()

let test_decorations () =
  let parser = Ansi.create () in
  let actual =
    "’Twas brillig, and the slithy toves
     \x1b[1mDid gyre and gimble in the wabe\x1b[0m;
     \x1b[3mAll mimsy were the borogoves\x1b[0m,
     \x1b[4mAnd the mome raths outgrabe\x1b[0m.

     \x1b[1m“Beware the \x1b[3mJabberwock, my son\x1b[0m!
     The jaws that bite, the claws that catch!"
    |> Ansi.process parser
  and expected =
    "’Twas brillig, and the slithy toves
     <span class='bold'>Did gyre and gimble in the wabe</span>;
     <span class='italic'>All mimsy were the borogoves</span>,
     <span class='underline'>And the mome raths outgrabe</span>.

     <span class='bold'>“Beware the </span><span class='italic bold'>Jabberwock, my son</span>!
     The jaws that bite, the claws that catch!"
  in
  Alcotest.(check string) "bold, italic, underline" expected actual

let () =
  let open Alcotest in
  run "ansi" [
      "utils", [
        test_case "Ansi.strip"     `Quick test_strip;
        test_case "decorations"    `Quick test_decorations;
      ];
    ]
