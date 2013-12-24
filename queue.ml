open Core.Std

module Queue: sig
  type 'a queue
  val empty: 'a queue
  val isEmpty: 'a queue -> bool
  val snoc: 'a queue * 'a -> 'a queue
  val head: 'a queue -> 'a
  val tail: 'a queue -> 'a queue
end = struct
  type 'a queue = 'a list * 'a list
  let empty = ([], [])
  let isEmpty = function
    | ([], []) -> true
    | _ -> false
  let snoc = function
    | ([], _), x -> ([x], [])
    | (f, r), x -> (f, x :: r)
  let head = function
    | (x :: _, _) -> x
    | _ -> failwith "apply head on empty queue"
  let tail = function
    | ([x], r) -> (List.rev r, [])
    | (_ :: f, r) -> (f, r)
    | _ -> failwith "apply head on empty queue"
end