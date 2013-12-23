type color = Red | Black

type 'a rbtree = Node of color * 'a * 'a rbtree * 'a rbtree | Leaf

let rec member x = function
  | Leaf -> false
  | Node (_, y, left, right) ->
      x = y || x < y && member x left || x > y && member x right

let balance = function
  | Node (Black, z, Node (Red, y, Node (Red, x, a, b), c), d)
  | Node (Black, z, Node (Red, x, a, Node (Red, y, b, c)), d)
  | Node (Black, x, a, Node (Red, z, Node (Red, y, b, c), d))
  | Node (Black, x, a, Node (Red, y, b, Node (Red, z, c, d))) ->
      Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
  | balanced -> balanced

let insert x t =
  let rec ins = function
    | Leaf -> Node (Red, x, Leaf, Leaf)
    | Node (color, y, left, right) as t ->
        if x < y then balance (Node (color, y, ins left, right))
        else if x > y then balance (Node (color, y, left, ins right))
        else t
  in
    match ins t with
      | Node (_, y, left, right) -> Node (Black, y, left, right)
      | Leaf -> failwith "rbtree insert returns Leaf"

let construct =
  List.fold_left ~init: Leaf ~f: (fun tree value -> insert value tree)

(* tests *)

let rbt = construct [1; 3; 5; 7; 9]

let () = assert (member 1 rbt)
let () = assert (not (member 2 rbt))

