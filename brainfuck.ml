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

let execute ?(array_size=30000) ?(loops_limit=10000000)
    str user_putchar user_getchar =
  let a = Array.make array_size 0 in
  let stub = (fun iters pointer i -> ()) in
  let code = Array.make (String.length str) stub in
  let rec aux_loop iters pointer i =
    if i < Array.length code then
      code.(i) iters pointer i
    else
      ()
  and plus iters pointer i =
    a.(pointer) <- succ a.(pointer);
    aux_loop iters pointer (succ i)
  and minus iters pointer i=
    a.(pointer) <- pred a.(pointer);
    aux_loop iters pointer (succ i)
  and shift_left iters pointer i=
    aux_loop iters (pred pointer) (succ i)
  and shift_right iters pointer i=
    aux_loop iters (succ pointer) (succ i)
  and putchar iters pointer i =
    user_putchar a.(pointer);
    aux_loop iters pointer (succ i)
  and getchar iters pointer i =
    a.(pointer) <- user_getchar ();
    aux_loop iters pointer i
  and start_loop jmp iters pointer i=
    if a.(pointer) <> 0 then
      if iters < loops_limit then
        aux_loop (succ iters) pointer (succ i)
      else
        raise (Error "Iteration limit exceed")
    else
      aux_loop iters pointer jmp
  and end_loop jmp iters pointer i =
    aux_loop iters pointer jmp
  and ign iters pointer i =
    aux_loop iters pointer (succ i)
  in
  let rec find_eparen deep i =
    if i < String.length str then
      match str.[i] with
        | '[' -> find_eparen (succ deep) (succ i) 
        | ']' -> if deep = 0 then i else find_eparen (pred deep) (succ i)
        | _ -> find_eparen deep (succ i)
    else
      raise (Error "Unmatched [")
  in
  let rec parse i =
    if i < String.length str then
      match str.[i] with
        | '+' -> code.(i) <- plus; parse (succ i)
        | '-' -> code.(i) <- minus; parse (succ i)
        | '<' -> code.(i) <- shift_left; parse (succ i)
        | '>' -> code.(i) <- shift_right; parse (succ i)
        | '.' -> code.(i) <- putchar; parse (succ i)
        | ',' -> code.(i) <- getchar; parse (succ i)
        | '[' ->
            let eparen = find_eparen 0 (succ i) in
              code.(i) <- start_loop (succ eparen);
              code.(eparen) <- end_loop i;
              parse (succ i)
        | ']' ->
            (*
            if code.(i) = stub then
              raise (Error "Unmatched ]")
            else
            *)
              parse (succ i)
        | _ -> code.(i) <- ign; parse (succ i)
    else
      ()
  in
    parse 0;
    aux_loop 0 0 0
