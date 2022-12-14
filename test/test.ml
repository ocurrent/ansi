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
  Alcotest.(check string) "strip escape sequences" expected actual

let () =
  let open Alcotest in
  run "ansi" [
      "utils", [
        test_case "Ansi.strip"     `Quick test_strip;
      ];
    ]
