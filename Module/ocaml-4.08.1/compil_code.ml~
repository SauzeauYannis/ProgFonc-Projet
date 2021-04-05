open Expression_scanner;;
open Stack;;

type operator = | Plus | Minus | Mult | Div;;
type tree =
  | Var of char
  | Cst of int
  | Unary of tree
  | Binary of operator * tree * tree;;

(* Conversion d'un token en operateur *)
let operator_of_token token =
  match token with
  | Add -> Plus
  | Subtract -> Minus
  | Multiply -> Mult
  | Divide -> Div
  | _ -> failwith "le token n'est pas convertible en opérateur";;

(* Analyse d'une expression de Lukasiewicz *)
let parse token_list =
  let stack : tree t = create() in
  let rec parse_aux(token_l : token list): tree =
    match token_l with
    | [] -> failwith "l'expression de Lukasiewicz est mal formée"
    | End::[] -> pop stack
    | hd::tl ->
       match hd with
       | Add | Subtract | Multiply | Divide ->
          let tree = Binary(operator_of_token hd, pop stack, pop stack) in
          push tree stack;
          parse_aux tl
       | Minus ->
          let tree = Unary(pop stack) in
          push tree stack;
          parse_aux tl
       | Variable(v) ->
          push (Var(v)) stack;
          parse_aux tl
       | Number(n) ->
          push (Cst(n)) stack;
          parse_aux tl
       | _ -> failwith "l'expression de Lukasiewicz est mal formée"
  in parse_aux token_list
;;

(* Evaluation d'une sous-expression de constantes entières *)
let evaluate operator n1 n2 =
  match operator with
  | Plus -> Cst(n1 + n2)
  | Minus -> Cst(n1 - n2)
  | Mult -> Cst(n1 * n2)
  | Div ->
     if n2 <> 0
     then Cst(n1 / n2)
     else Binary(operator, Cst(n1), Cst(n2))
;;

(* Evaluation d'une sous-expression de variables *)
let eval_sub_expr operator x y =
  if x = y
  then
    match operator with
    | Minus -> Cst(0)
    | Div -> Cst(1)
    | _ -> Binary(operator, x, y)
  else
    match (operator, x, y) with
    | (Mult, Cst(1), z)
      | (Mult, z, Cst(1))
      | (Plus, Cst(0), z)
      | (Plus, z, Cst(0)) -> z
    | (Mult, Cst(0), _)
      | (Mult, _, Cst(0)) -> Cst(0)
    | _ -> Binary(operator, x, y)
;;

(* Evaluation d'une sous-expression *)
let rec simplification tree =
  match tree with
  | Binary(operator, x, y) -> (
    match (simplification x, simplification y) with
    | (Cst(n1), Cst(n2)) -> evaluate operator n1 n2
    | (simpl_x, simpl_y) -> eval_sub_expr operator simpl_x simpl_y)
  | _ -> tree
;;

(* Ouvre le module Printf pour effectuer un affichage sur un terminal *)
open Printf;;

(* Conversion un operateur en string *)
let string_of_operator op =
  match op with
  | Plus -> " + "
  | Minus -> " - "
  | Mult -> " * "
  | Div -> " / "
;;

(* Ouverture du module Char pour utiliser la fonction 'escaped' *)
open Char;;

(* Conversion d'un arbre en string sans simplifiaction des parenthèses *)
let string_of_tree tree =
  let rec string_of_tree_aux tree first =
    match tree with
    | Cst(n) -> string_of_int n
    | Var(x) -> escaped x
    | Unary(exp) -> "(-" ^ (string_of_tree_aux exp false) ^ ")"
    | Binary(op, x, y) ->
       if first
       then (string_of_tree_aux x false) ^ (string_of_operator op) ^ (string_of_tree_aux y false)
       else "(" ^ (string_of_tree_aux x false) ^ (string_of_operator op) ^ (string_of_tree_aux y false) ^ ")"
  in string_of_tree_aux tree true
;;

(* Conversion d'un arbre en string avec simplifiaction des parenthèses *)
let rec string_of_tree_final tree =
  match tree with
  | Cst(n) -> string_of_int n
  | Var(x) -> escaped x
  | Unary(exp) -> (
    match exp with
    | Cst(_) | Var(_) -> "(-" ^ (string_of_tree_final exp) ^ ")"
    | _ -> "(-(" ^ (string_of_tree_final exp) ^ "))")
  | Binary(op, x, y) ->
     match op with
     | Plus | Minus -> (string_of_tree_final x) ^ (string_of_operator op) ^ (string_of_tree_final y)
     | _ ->
        match (x, y) with
        | (Binary(Plus,_,_), _) | (Binary(Minus,_,_), _) -> "(" ^ (string_of_tree_final x) ^ ")" ^ (string_of_operator op) ^ (string_of_tree_final y)
        | (_, Binary(Plus,_,_)) | (_, Binary(Minus,_,_)) -> (string_of_tree_final x) ^ (string_of_operator op) ^ "(" ^ (string_of_tree_final y) ^ ")"
        | _ -> (string_of_tree_final x) ^ (string_of_operator op) ^ (string_of_tree_final y)
;;

(* Affichage sur le terminal de l'expression sans simplifiaction des parenthèses *)
let print_exp tree =
  printf "%s\n" (string_of_tree tree)
;;

(* Affichage sur le terminal de l'expression avec simplifiaction des parenthèses *)
let print_exp_final tree =
  printf "%s\n" (string_of_tree_final tree)
;;

(* ----------------------------------- Programme final ----------------------------------- *)

(* Ouvre le module Printf pour effectuer un affichage sur un terminal *)
open List;;

(* Fonction principale *)
let main =
  let input_list = input_to_token_list() in
  let (temp_list, token_list) =
    fold_right (fun elem (sub_list, list) ->
        if elem = End && sub_list <> []
        then ([End], sub_list::list)
        else (elem::sub_list, list))
      input_list ([], []) in
  let num_exp = ref 1 in
  map (fun elem ->
      let tree = parse elem in
      printf "Expression numéro %d avant simplification :\n" !num_exp;
      print_exp tree;
      let simpl_tree = simplification tree in
      printf "Expression numéro %d après simplification :\n" !num_exp;
      print_exp_final simpl_tree;
      printf "\n";
      num_exp := !num_exp + 1;)
  (temp_list::token_list)
;;

main();
