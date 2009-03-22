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

let execute ?(array_size=30000) code putchar getchar =
  let a = Array.make array_size 0 in
  let rec skip_loop deep i =
    if i < String.length code then
      match code.[i] with
        | '[' -> skip_loop (deep+1) (i+1)
        | ']' -> if deep = 0 then i else skip_loop (deep-1) (i+1)
        | _ -> skip_loop deep (i+1)
    else
      i
  in
  let rec aux_loop pointer i =
    if i < String.length code then
      match code.[i] with
        | '>' ->
            aux_loop (pointer + 1) (i+1)
        | '<' ->
            aux_loop (pointer - 1) (i+1)
        | '+' ->
            a.(pointer) <- a.(pointer) + 1;
            aux_loop pointer (i+1)
        | '-' ->
            a.(pointer) <- a.(pointer) - 1;
            aux_loop pointer (i+1)
        | '.' ->
            putchar (Char.chr a.(pointer));
            aux_loop pointer (i+1)
        | ',' ->
            a.(pointer) <- Char.code (getchar ());
            aux_loop pointer (i+1)
        | '[' ->
            if a.(pointer) <> 0 then
              let newpointer = aux_loop pointer (i+1) in
                aux_loop newpointer i
            else
              let i = skip_loop 0 (i+1) in
                aux_loop pointer (i+1)
        | ']' ->
            pointer
        | _ ->
            aux_loop pointer (i+1)
    else
      pointer
  in
    aux_loop 0 0
