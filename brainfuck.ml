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

let execute ?(array_size=30000) ?(loops_limit=1000000) code putchar getchar =
  let a = Array.make array_size 0 in
  let rec skip_loop deep i =
    if i < String.length code then
      match code.[i] with
        | '[' -> skip_loop (succ deep) (succ i)
        | ']' -> if deep = 0 then i else skip_loop (pred deep) (succ i)
        | _ -> skip_loop deep (succ i)
    else
      i
  in
  let rec aux_loop looping iters pointer i =
    if i < String.length code then
      match code.[i] with
        | '>' ->
            aux_loop looping iters (succ pointer) (succ i)
        | '<' ->
            aux_loop looping iters (pred pointer) (succ i)
        | '+' ->
            a.(pointer) <- succ a.(pointer);
            aux_loop looping iters pointer (succ i)
        | '-' ->
            a.(pointer) <- pred a.(pointer);
            aux_loop looping iters pointer (succ i)
        | '.' ->
            putchar (Char.chr a.(pointer));
            aux_loop looping iters pointer (succ i)
        | ',' ->
            a.(pointer) <- Char.code (getchar ());
            aux_loop looping iters pointer (succ i)
        | '[' ->
            if a.(pointer) <> 0 then
              if iters < loops_limit then
                let _, newpointer = aux_loop true iters pointer (succ i) in
                  aux_loop looping (succ iters) newpointer i
              else
                raise (Error "Iteration limit exceed")
            else
              let i = skip_loop 0 (succ i) in
                aux_loop looping iters pointer (succ i)
        | ']' ->
            if looping then
              looping, pointer
            else
              raise (Error "Unmatched ]")
        | _ ->
            aux_loop looping iters pointer (succ i)
    else
      looping, pointer
  in
  let looping, _ = aux_loop false 0 0 0 in
    if looping then
      raise (Error "Unmatched [")
