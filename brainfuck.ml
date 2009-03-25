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

type code = int -> (int * int) list -> int -> int -> code array -> unit

let execute ?(array_size=30000) ?(loops_limit=1000000)
    str user_putchar user_getchar =
  let a = Array.make array_size 0 in
  let rec aux_loop (iters:int) (loops:(int * int) list) (pointer:int)
      (i:int) (code:code array) =
    if i < Array.length code then
      code.(i) iters loops pointer i code
    else
      ()
  and plus iters loops pointer i code =
    a.(pointer) <- succ a.(pointer);
    aux_loop iters loops pointer (succ i) code
  and minus iters loops pointer i code =
    a.(pointer) <- pred a.(pointer);
    aux_loop iters loops pointer (succ i) code
  and shift_left iters loops pointer i code =
    aux_loop iters loops (pred pointer) (succ i) code
  and shift_right iters loops pointer i code =
    aux_loop iters loops (succ pointer) (succ i) code
  and putchar iters loops pointer i code =
    user_putchar (Char.chr a.(pointer));
    aux_loop iters loops pointer (succ i) code
  and getchar iters loops pointer i code =
    a.(pointer) <- Char.code (user_getchar ());
    aux_loop iters loops pointer i code
  and start_loop iters loops pointer i code =
    if a.(pointer) <> 0 then
      if iters < loops_limit then
        aux_loop (pred iters) loops pointer (succ i) code
      else
        raise (Error "Iteration limit exceed")
    else
      let (_, i) = List.hd loops in
        aux_loop iters (List.tl loops) pointer i code
  and end_loop iters loops pointer i code =
    let (i, _) = List.hd loops in
      aux_loop iters loops pointer i code
  and ign iters loops pointer i code =
    aux_loop iters loops pointer (succ i) code
  in
  let rec find_eparen deep i =
    if i < String.length str then
      match str.[i] with
        | '[' -> find_eparen (succ deep) (succ i) 
        | ']' -> if deep = 0 then succ i else find_eparen (pred deep) (succ i)
        | _ -> find_eparen deep (succ i)
    else
      raise (Error "Unmatched [")
  in
  let rec parse code loops i =
    if i < String.length str then
      match str.[i] with
        | '+' -> parse (plus :: code) loops (succ i)
        | '-' -> parse (minus :: code) loops (succ i)
        | '<' -> parse (shift_left :: code) loops (succ i)
        | '>' -> parse (shift_right :: code) loops (succ i)
        | '.' -> parse (putchar :: code) loops (succ i)
        | ',' -> parse (getchar :: code) loops (succ i)
        | '[' ->
            let eparen = find_eparen 0 (succ i) in
              parse (start_loop :: code) ((i, eparen) :: loops) (succ i)
        | ']' ->
            parse (end_loop :: code) loops (succ i)
        | _ -> parse (ign::code) loops (succ i)
    else
      Array.of_list (List.rev code), List.rev loops
  in
  let code, loops = parse [] [] 0 in
    aux_loop 0 loops 0 0 code
