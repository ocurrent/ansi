let () =
  let title = if Array.length Sys.argv >= 2 then Printf.sprintf {|<title>%s</title>|} Sys.argv.(1) else "" in
  Printf.printf {|<!DOCTYPE html><html><head><meta charset="UTF-8">%s<style>
%s</style></head><body><pre>|} title Ansi.css;
  let p = Ansi.create () in
  begin try
    while true do
      read_line ()
      |> Ansi.process p
      |> print_endline
    done
    with End_of_file -> ()
  end;
  print_endline {|</pre></body></html>|}
