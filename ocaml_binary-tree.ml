(* ocaml_binary-tree.ml *)
(* Version of Sat 01 Oct 2017 *)

(* ********** *)

(*
   name: Jianglong Liao
   e-mail address: jlliao@u.yale-nus.edu.sg
*)


(* ********** *)

let show_bool b =
  if b
  then "true"
  else "false";;

let show_int n =
  if n < 0
  then "(" ^ string_of_int n ^ ")"
  else string_of_int n;;

(* ********** *)

type binary_tree' =
  | Leaf'
  | Node' of binary_tree' * int * binary_tree';;

let rec show_binary_tree' t =
  match t with
  | Leaf' ->
    "Leaf'"
  | Node' (t1, n, t2) ->
     "Node' (" ^ show_binary_tree' t1 ^ ", " ^ show_int n ^ ", " ^ show_binary_tree' t2 ^ ")";;

(* ********** *)

(* Question 2.1 *)

let test_count_leaf candidate_leaf =
  (* test_count_leaf : (binary_tree' -> int) -> bool *)
    (candidate_leaf (Leaf')
    = 1)
  &&
    (candidate_leaf (Node' (Leaf', 1, Leaf'))
    = 2)
  &&
    (candidate_leaf (Node' (Node' (Leaf', 2, 
                     Leaf')
                    , 1, Leaf'))
    = 3)
  &&
    (candidate_leaf (Node' (Leaf', 1,
                (Node' (Leaf', 2, 
                     Leaf'))))
    = 3)
  &&
    (candidate_leaf (Node' (Node' (Leaf', 2,
                     Leaf'), 1,
                 (Node' (Leaf', 3,
                     Leaf'))))
    = 4)
    (* addition test by Kota *)
  &&
      (candidate_leaf (Node'(Node'(Node'(Leaf', 4, Leaf'), 2, 
                            Leaf' ),1,
                        Node'(Leaf', 3, 
                            Node'(Leaf', 5, 
                                Leaf'))))
      = 6)
    (* etc. *);;

let rec count_leaf t =
  (* count_leaf : binary_tree' -> int *)
  match t with
  | Leaf' -> 
    1
  | Node' (t1, n, t2) ->
    let n1 = count_leaf t1
    and n2 = count_leaf t2
  in n1 + n2;;

let () = assert(test_count_leaf count_leaf);;

(* ********** *)

(* Question 2.2 *)

let test_count_node candidate_node =
  (* test_count_leaf : (binary_tree' -> int) -> bool *)
    (candidate_node (Leaf')
    = 0)
  &&
    (candidate_node (Node' (Leaf', 1, Leaf'))
    = 1)
  &&
    (candidate_node (Node' (Node' (Leaf', 2, 
                     Leaf')
                    , 1, Leaf'))
    = 2)
  &&
    (candidate_node (Node' (Leaf', 1,
                (Node' (Leaf', 2, 
                     Leaf'))))
    = 2)
  &&
    (candidate_node (Node' (Node' (Leaf', 2,
                     Leaf'), 1,
                 (Node' (Leaf', 3,
                     Leaf'))))
    = 3)
    (* end Jianglong & Toby *)
  &&
      (* additional test by Kota *)
      (candidate_node (Node'(Node'(Node'(Leaf', 4,
                         Leaf'), 2, 
                          Leaf' ),1,
                        Node'(Leaf', 3, 
                            Node'(Leaf', 5,
                               Leaf'))))
    = 5 )
    (* etc. *);;

let rec count_node t =
  (* count_node : binary_tree' -> int *)
  match t with
  | Leaf' -> 
    0
  | Node' (t1, n, t2) ->
    let n1 = count_node t1
    and n2 = count_node t2
  in n1 + n2 + 1;;

let () = assert(test_count_node count_node);;

(* ********** *)

(* Question 2.4 *)

let test_left_balanced' candidate =
    (candidate Leaf'
     = true )
  &&
    (candidate (Node' (Leaf',
                       1,
                       Leaf'))
     = true )
  &&
    (candidate (Node' (Node' (Leaf',
                              2,
                              Leaf'),
                       1,
                       Leaf'))
     = true )
  &&
    (candidate (Node' (Node' (Node' (Leaf',
                                     3,
                                     Leaf'),
                              2,
                              Leaf'),
                       1,
                       Leaf'))
     = true )
  &&
    (candidate (Node' (Node' (Node' (Node' (Leaf',
                                            3,
                                            Leaf'),
                                     2,
                                     Leaf'),
                              1,
                              Leaf'),
                       0,
                      Leaf'))
     = true )
  &&
    (candidate (Node' (Node' (Leaf',
                              1,
                              Leaf'),
                       3,
                       Node' (Leaf',
                              0,
                              Leaf')))
     = false )
    &&
    (candidate (Node' (Node' (Leaf', 2,
                 Leaf'), 1,
           (Node' (Leaf', 3,
               Leaf'))))
     = false)
  (* etc. *);;

let rec left_balanced' t = 
  (* left_balanced' : binary_tree' -> bool *)
  match t with
  | Leaf' ->
    true
  | Node' (t1, n, t2) ->
    let b1 = left_balanced' t1
    and b2 = (match t2 with
          | Leaf' ->
          true
          | Node' _ ->
          false)
    in b1 && b2;;

let() = assert (test_left_balanced' left_balanced');;

(* ********** *)

(* Question 2.5 *)

let test_right_balanced' candidate_right =
  (* test given by instruction *)
  (candidate_right (Leaf')
    = true)
  &&
  (candidate_right (Node' (Leaf', 1, 
            Leaf'))
    = true)
  &&
  (candidate_right (Node' (Leaf', 1, 
                (Node' (Leaf', 2, 
                Leaf'))))
    = true)
  &&
  (candidate_right (Node' (Leaf', 1, 
                (Node' (Leaf', 2, 
                    (Node' (Leaf', 3,
                        Leaf'))))))
    = true)
  &&
  (candidate_right (Node' (Leaf', 1, 
                (Node' (Leaf', 2, 
                    (Node' (Leaf', 3,
                        (Node' (Leaf', 4,
                            Leaf'))))))))
    = true)
  &&
  (candidate_right (Node' (Node' (Leaf', 2,
                 Leaf'), 1,
           (Node' (Leaf', 3,
               Leaf'))))
    = false)
  (* etc *);;

let rec right_balanced' t = 
  (* right_balanced' : binary_tree' -> bool *)
  match t with
  | Leaf' ->
    true
  | Node' (t1, n, t2) ->
    let b2 = right_balanced' t2
    and b1 = (match t1 with
          | Leaf' ->
          true
          | Node' _ ->
          false)
    in b1 && b2;;

let() = assert(test_right_balanced' right_balanced');;


(* ********** *)

(* Part 3 *)

type left_binary_tree' =
  | Left_Leaf'
  | Left_Node' of left_binary_tree' * int;;

let rec show_left_binary_tree' t =
  match t with
  | Left_Leaf' ->
    "Left_Leaf'"
  | Left_Node' (t1, n) ->
     "Left_Node' (" ^ show_left_binary_tree' t1 ^ ", " ^ show_int n ^ ")";;

let rec embed_left_binary_tree'_into_binary_tree' t =
     (* embed_left_binary_tree'_into_binary_tree' : left_binary_tree' -> binary_tree' *)
  match t with
  | Left_Leaf' ->
     Leaf'
  | Left_Node' (t1, n) ->
     Node' (embed_left_binary_tree'_into_binary_tree' t1, n, Leaf');;

type option_left_binary_tree' =
  | Some_left_binary_tree' of left_binary_tree'
  | None_left_binary_tree';;

let rec project_binary_tree'_into_left_binary_tree' t =
     (* project_binary_tree'_into_left_binary_tree' : binary_tree' -> option_left_binary_tree' *)
  match t with
  | Leaf' ->
     Some_left_binary_tree' Left_Leaf'
  | Node' (t1, n, t2) ->
     match t2  with
     | Leaf' ->
        (match project_binary_tree'_into_left_binary_tree' t1 with
         | Some_left_binary_tree' t1' ->
            Some_left_binary_tree' (Left_Node' (t1', n))
         | None_left_binary_tree' ->
            None_left_binary_tree')
     | Node' _ ->
        None_left_binary_tree';;

(* ********** *)


(* Question 3.3 *)

let test_left_stitch candidate =
 (* test_left_stitch : (left_binary_tree' -> left_binary_tree' -> left_binary_tree') -> bool *)
  (candidate Left_Leaf'
             Left_Leaf'
   = Left_Leaf')
  &&
  (candidate (Left_Node' (Left_Leaf', 1))
             Left_Leaf'
   = Left_Node' (Left_Leaf', 1))
  &&
  (candidate Left_Leaf'
             (Left_Node' (Left_Leaf', 1))
   = Left_Node' (Left_Leaf', 1))
  &&
  (candidate (Left_Node' (Left_Node' (Left_Leaf', 2), 1))
             Left_Leaf'
   = Left_Node' (Left_Node' (Left_Leaf', 2), 1))
  &&
  (candidate Left_Leaf'
             (Left_Node' (Left_Node' (Left_Leaf', 2), 1))
   = Left_Node' (Left_Node' (Left_Leaf', 2), 1))
  &&
  (candidate (Left_Node' (Left_Node' (Left_Leaf', 4), 3))
             (Left_Node' (Left_Node' (Left_Leaf', 2), 1))
   = Left_Node' (Left_Node' (Left_Node' (Left_Node' (Left_Leaf', 4), 3), 2), 1))
  &&
  (* the pictorial example *)
  (candidate (Left_Node' (Left_Node' (Left_Leaf', 5), 4))
             (Left_Node' (Left_Node' (Left_Node' (Left_Leaf', 3), 2), 1))
   = Left_Node' (Left_Node' (Left_Node' (Left_Node' (Left_Node' (Left_Leaf', 5), 4), 3), 2), 1))
  (* etc. *);;

let rec left_stitch t1 t2 =
    (* left_stich: left_binary_tree' -> left_binary_tree' -> left_binary_tree' *)  
  match t2 with
  | Left_Leaf' ->
    t1
  | Left_Node' (t2', n) ->
      let t3 = left_stitch t1 t2'
    in Left_Node' (t3, n);;

let() =assert(test_left_stitch left_stitch);;

(* ********** *)

(* Question 3.4 *)

let test_left_rotate candidate =
 (* test_left_rotate : (binary_tree' -> left_binary_tree') -> bool *)
  (candidate Leaf'
   = Left_Leaf')
  &&
  (candidate (Node' (Leaf',
                     1,
                     Leaf'))
   = Left_Node' (Left_Leaf',
                 1))
  &&
  (candidate (Node' (Node' (Leaf',
                            2,
                            Leaf'),
                     1,
                     Node' (Leaf',
                            3,
                            Leaf')))
   = Left_Node' (Left_Node' (Left_Node' (Left_Leaf',
                                         2),
                             1),
                 3))
  &&
  (candidate (Node' (Node' (Node' (Leaf',
                                   4,
                                   Leaf'),
                            2,
                            Node' (Leaf',
                                   5,
                                   Leaf')),
                     1,
                     Node' (Node' (Leaf',
                                   6,
                                   Leaf'),
                            3,
                            Node' (Leaf',
                                   7,
                                   Leaf'))))
   = Left_Node' (Left_Node' (Left_Node' (Left_Node' (Left_Node' (Left_Node' (Left_Node' (Left_Leaf',
                                                                                         4),
                                                                             2),
                                                                 5),
                                                     1),
                                         6),
                             3),
                 7))
  (* etc. *);;

let rec left_rotate t =
  match t with
  | Leaf' ->
    Left_Leaf'
  | Node' (t1, n, t2) ->
    let lt1 = left_rotate t1
    and lt2 = left_rotate t2
  in left_stitch (Left_Node' (lt1, n)) lt2
;;

let () = assert (test_left_rotate left_rotate);;

(* ********** *)

(* Question 3.5 *)

let test_left_flatten candidate =
 (* test_left_flatten : (binary_tree' -> binary_tree') -> bool *)
    (candidate Leaf'
   = Leaf')
  &&
  (candidate (Node' (Leaf',
                     1,
                     Leaf'))
   = Node' (Leaf',
                 1, Leaf'))
  &&
   (candidate (Node' (Node' (Leaf',
                            2,
                            Leaf'),
                     1,
                     Node' (Leaf',
                            3,
                            Leaf')))
   = Node' (Node' (Node' (Leaf',
                            2, Leaf'),
                             1, Leaf'),
                 3, Leaf'))
  &&
  (candidate (Node' (Node' (Node' (Leaf',
                                   4,
                                   Leaf'),
                            2,
                            Node' (Leaf',
                                   5,
                                   Leaf')),
                     1,
                     Node' (Node' (Leaf',
                                   6,
                                   Leaf'),
                            3,
                            Node' (Leaf',
                                   7,
                                   Leaf'))))
   = Node' (Node' (Node' (Node' (Node' (Node' (Node' (Leaf',
                                                        4, Leaf'),
                                              2, Leaf'),
                                       5, Leaf'),
                                1, Leaf'),
                         6, Leaf'),
                  3, Leaf'),
          7, Leaf'))

  (* etc. *);;


let left_flatten t =
 (* left_flatten : binary_tree' -> binary_tree' *)
  embed_left_binary_tree'_into_binary_tree' (left_rotate t);;



let () = assert (test_left_flatten left_flatten);;


(* ********** *)

(* Part 4 *)

type right_binary_tree' =
  | Right_Leaf'
  | Right_Node' of int * right_binary_tree';;

let rec show_right_binary_tree' t =
  match t with
  | Right_Leaf' ->
    "Right_Leaf'"
  | Right_Node' (n, t1) ->
     "Right_Node' (" ^ show_int n ^ ", " ^ show_right_binary_tree' t1 ^ ")";;

let rec embed_right_binary_tree'_into_binary_tree' t =
     (* embed_right_binary_tree'_into_binary_tree' : right_binary_tree' -> binary_tree' *)
  match t with
  | Right_Leaf' ->
     Leaf'
  | Right_Node' (n, t1) ->
     Node' (Leaf', n, embed_right_binary_tree'_into_binary_tree' t1);;

type option_right_binary_tree' =
  | Some_right_binary_tree' of right_binary_tree'
  | None_right_binary_tree';;

let rec project_binary_tree'_into_right_binary_tree' t =
     (* project_binary_tree'_into_right_binary_tree' : binary_tree' -> option_right_binary_tree' *)
  match t with
  | Leaf' ->
     Some_right_binary_tree' Right_Leaf'
  | Node' (t1, n, t2) ->
     match t1  with
     | Leaf' ->
        (match project_binary_tree'_into_right_binary_tree' t2 with
         | Some_right_binary_tree' t2' ->
            Some_right_binary_tree' (Right_Node' (n, t2'))
         | None_right_binary_tree' ->
            None_right_binary_tree')
     | Node' _ ->
        None_right_binary_tree';;


(* ********** *)

(* Question 4.3 *)

let test_right_stitch candidate =
 (* test_right_stitch : (right_binary_tree' -> right_binary_tree'_binary_tree' -> right_binary_tree') -> bool *)
  (candidate Right_Leaf'
             Right_Leaf'
   = Right_Leaf')
  &&
  (candidate (Right_Node' (1, Right_Leaf'))
             Right_Leaf'
   = Right_Node' (1, Right_Leaf'))
  &&
  (candidate Right_Leaf'
             (Right_Node' (1, Right_Leaf'))
   = Right_Node' (1, Right_Leaf'))
  &&
  (candidate (Right_Node' (1, Right_Node' (2, Right_Leaf')))
             Right_Leaf'
   = Right_Node' (1, Right_Node' (2, Right_Leaf')))
  &&
  (candidate Right_Leaf'
             (Right_Node' (1, Right_Node' (2, Right_Leaf')))
   = Right_Node' (1, Right_Node' (2, Right_Leaf')))
  &&
  (candidate (Right_Node' (1, Right_Node' (2, Right_Leaf')))
             (Right_Node' (3, Right_Node' (4, Right_Leaf')))
   = Right_Node' (1, Right_Node' (2, Right_Node' (3, Right_Node' (4, Right_Leaf')))))
  (* etc. *);;

let rec right_stitch t1 t2 =
  match t1 with
  | Right_Leaf' ->
    t2
  | Right_Node' (n, t1') ->
      let t3 = right_stitch t1' t2
    in Right_Node' (n, t3);;


let () = assert (test_right_stitch right_stitch);;

(* ********** *)

(* Question 4.4 *)

let test_right_rotate candidate =
 (* test_right_rotate : (binary_tree' -> right_binary_tree') -> bool *)
  (candidate Leaf'
   = Right_Leaf')
  &&
  (candidate (Node' (Leaf',
                     1,
                     Leaf'))
   = Right_Node' (1,
                  Right_Leaf'))
  &&
  (candidate (Node' (Node' (Leaf',
                            2,
                            Leaf'),
                     1,
                     Node' (Leaf',
                            3,
                            Leaf')))
   = Right_Node' (2,
                  Right_Node' (1,
                               Right_Node' (3,
                                            Right_Leaf'))))
  &&
  (candidate (Node' (Node' (Node' (Leaf',
                                   4,
                                   Leaf'),
                            2,
                            Node' (Leaf',
                                   5,
                                   Leaf')),
                     1,
                     Node' (Node' (Leaf',
                                   6,
                                   Leaf'),
                            3,
                            Node' (Leaf',
                                   7,
                                   Leaf'))))
   = Right_Node' (4,
                  Right_Node' (2,
                               Right_Node' (5,
                                            Right_Node' (1,
                                                         Right_Node' (6,
                                                                      Right_Node' (3,
                                                                                   Right_Node' (7,
                                                                                                Right_Leaf'))))))))
  (* etc. *);;

let rec right_rotate t =
  match t with
   | Leaf' -> Right_Leaf'
   | Node' (t1, n, t2) ->
    let rt1 = right_rotate t1
    and rt2 = right_rotate t2
   in right_stitch rt1 (Right_Node' (n, rt2));;

let () = assert (test_right_rotate right_rotate);;

(* ********** *)

(* Question 4.5 *)

let test_right_flatten candidate= 
 (* Toby *)
 (candidate Leaf' = Leaf')
 &&
 (* Jianglong *)
 (candidate (Node'(Leaf', 1, Leaf'))=
(Node'(Leaf', 1, Leaf')))
 &&
 (* Kota *)
 (candidate (Node'(Node'(Leaf',2, Node'(Leaf', 5,Leaf')), 1, 
  Node'(Leaf',3, Node'(Leaf', 4, Leaf'))))
=  (Node'(Leaf', 2, Node'(Leaf', 5, Node'(Leaf', 1, Node'(Leaf', 3, Node'(Leaf',4, Leaf')))))))
    (* etc *)    ;;


let right_flatten t =
 (* right_flatten : binary_tree' -> binary_tree' *)
  embed_right_binary_tree'_into_binary_tree' (right_rotate t);;


let () = assert (test_right_flatten right_flatten);;


(* ********** *)

(* Part 5 *)

(* Question 5.1 *)

let test_left_to_right candidate =
 (* test_left_to_right : (left_binary_tree' -> right_binary_tree') -> bool *)
  (candidate Left_Leaf'
   = Right_Leaf')
  &&
  (candidate (Left_Node' (Left_Leaf', 1))
   = Right_Node' (1, Right_Leaf'))
  &&
  (candidate (Left_Node' (Left_Node' (Left_Leaf', 2), 1))
    = Right_Node' (2, Right_Node' (1, Right_Leaf')))
  &&
  (candidate (Left_Node' (Left_Node' (Left_Node' (Left_Leaf', 3), 2), 1))
    = Right_Node' (3, Right_Node' (2, Right_Node' (1, Right_Leaf'))))
  (* etc. *);;

let left_to_right t =
 (* left_flatten : binary_tree' -> binary_tree' *)
  right_rotate (embed_left_binary_tree'_into_binary_tree' t);;


let () = assert (test_left_to_right left_to_right);;


(* ********** *)

(* Question 5.2 *)

let test_right_to_left candidate =
 (* test_right_to_left : (right_binary_tree' -> left_binary_tree') -> bool *)
  (candidate Right_Leaf'
   = Left_Leaf')
  &&
  (candidate (Right_Node' (1, Right_Leaf'))
   = Left_Node' (Left_Leaf', 1))
  &&
  (candidate (Right_Node' (2, Right_Node' (1, Right_Leaf')))
   = Left_Node' (Left_Node' (Left_Leaf', 2), 1))
  &&
  (candidate (Right_Node' (3, Right_Node' (2, Right_Node' (1, Right_Leaf'))))
   = Left_Node' (Left_Node' (Left_Node' (Left_Leaf', 3), 2), 1))
  (* etc. *);;

  let right_to_left t =
 (* left_flatten : binary_tree' -> binary_tree' *)
  left_rotate (embed_right_binary_tree'_into_binary_tree' t);;


let () = assert (test_right_to_left right_to_left);;


(* ********** *)

(* Question 5.3 *)


let foo t
  = left_to_right (right_to_left t);;

let bar t
  = right_to_left (left_to_right t);;


(* ********** *)

(* Question 5.4 *)

let () = assert (test_left_rotate (fun t -> right_to_left (right_rotate t)));;


let () = assert (test_right_rotate (fun t -> left_to_right (left_rotate t)));;


(* ********** *)

(* end of week-07_midterm-project.ml *)

"ocaml_binary-tree.ml"
