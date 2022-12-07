let test_strip () =
  let actual =
    (String.concat "\n"
       (List.map Ansi.strip [
            "\027[34mthe lazy fox\027[39m jumps over the brown dog\027[0m";
            "the lazy fox \027[34mjumps over\027[39m the brown dog\027[0m";
            "\027[34mthe lazy fox\027[39m jumps over \027[0mthe brown dog";
            "\027[34mthe lazy fox \027[39mjumps over\027[0thebrown dog"
    ]));
  and expected =
    {|the lazy fox jumps over the brown dog
the lazy fox jumps over the brown dog
the lazy fox jumps over the brown dog
the lazy fox jumps over|}
  in
  Alcotest.(check string) "strip escape sequences" expected actual

let () =
  let open Alcotest in
  run "ansi" [
      "utils", [
        test_case "Ansi.strip"     `Quick test_strip;
      ];
    ]
