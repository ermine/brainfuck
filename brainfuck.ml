(*
 * (c) 2009 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

(*
 * Cmd  Effect
 * ---  ------
 * +    Increases element under pointer
 * -    Decrases element under pointer
 * >    Increases pointer
 * <    Decreases pointer
 * [    Starts loop, flag under pointer
 * ]    Indicates end of loop
 * .    Outputs ASCII code under pointer
 * ,    Reads char and stores ASCII under ptr
 *)

let execute program putchar getchar =
  let a = Array.make 3000 0 in
  let rec skip_loop deep i =
    if i < String.length program then
      match program.[i] with
        | '[' -> skip_loop (deep+1) (i+1)
        | ']' -> if deep = 0 then i else skip_loop (deep-1) (i+1)
        | _ -> skip_loop deep (i+1)
    else
      i
  in
  let rec aux_loop pointer i j =
    if i < String.length program then
      match program.[i] with
        | '>' ->
            aux_loop (pointer + 1) (i+1) j
        | '<' ->
            aux_loop (pointer - 1) (i+1) j
        | '+' ->
            a.(pointer) <- a.(pointer) + 1;
            aux_loop pointer (i+1) j
        | '-' ->
            a.(pointer) <- a.(pointer) - 1;
            aux_loop pointer (i+1) j
        | '.' ->
            putchar (Char.chr a.(pointer));
            aux_loop pointer (i+1) j
        | ',' ->
            a.(pointer) <- Char.code (getchar ());
            aux_loop pointer (i+1) j
        | '[' ->
            if a.(pointer) <> 0 then
              aux_loop pointer (i+1) i
            else
              let i = skip_loop 0 (i+1) in
                aux_loop pointer (i+1) j
        | ']' ->
            aux_loop pointer j j
        | _ ->
            aux_loop pointer (i+1) j
    else
      ()
  in
    aux_loop 0 0 0
    
let _ =
  let fin = open_in Sys.argv.(1) in
  let buf = Buffer.create 1024 in
  let () =
    try while true do Buffer.add_string buf (input_line fin) done
    with End_of_file -> close_in fin in
    
  let getchar () = input_char stdin in
  let putchar ch = print_char ch in
    execute (Buffer.contents buf) putchar getchar
