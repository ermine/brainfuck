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

exception Error of string

let parse str =
  let rec find_eparen deep i =
    if i < String.length str then
      match str.[i] with
        | '[' -> find_eparen (succ deep) (succ i)
        | ']' -> if deep = 0 then i else find_eparen (pred deep) (succ i)
        | _ -> find_eparen deep (succ i)
    else
      raise (Error "Unmatched [")
  in
  let rec aux_parse ps i =
    if i < String.length str then
      match str.[i] with
        | '[' ->
            let eparen = find_eparen 0 (succ i) in
              aux_parse ((i, eparen) :: ps) (succ i)
        | _ -> aux_parse ps (succ i)
    else
      List.rev ps
  in
    aux_parse [] 0
    
let execute ?(array_size=30000) ?(loops_limit=1000000) code putchar getchar =
  let a = Array.make array_size 0 in
  let rec aux_loop ps iters pointer i =
    if i < String.length code then
      match code.[i] with
        | '>' ->
            aux_loop ps iters (succ pointer) (succ i)
        | '<' ->
            aux_loop ps iters (pred pointer) (succ i)
        | '+' ->
            a.(pointer) <- succ a.(pointer);
            aux_loop ps iters pointer (succ i)
        | '-' ->
            a.(pointer) <- pred a.(pointer);
            aux_loop ps iters pointer (succ i)
        | '.' ->
            putchar (Char.chr a.(pointer));
            aux_loop ps iters pointer (succ i)
        | ',' ->
            a.(pointer) <- Char.code (getchar ());
            aux_loop ps iters pointer (succ i)
        | '[' ->
            if a.(pointer) <> 0 then
              if iters < loops_limit then
                aux_loop ps iters pointer (succ i)
              else
                raise (Error "Iteration limit exceed")
            else
              let (_, i) = List.hd ps in
                aux_loop (List.tl ps) iters pointer (succ i)
        | ']' ->
            let (i, _) = List.hd ps in
              aux_loop ps iters pointer i
        | _ ->
            aux_loop ps iters pointer (succ i)
    else
      ()
  in
  let ps = parse code in
    aux_loop ps 0 0 0
