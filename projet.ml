(* D�commenter la commande directory selon votre verison d'ocaml *)

(* #directory "module/ocaml-4.02.1+ocp1/";; *)

(* #directory "module/ocaml-4.02.3/";; *)

(* #directory "module/ocaml-4.05.0/";; *)

(* #directory "module/ocaml-4.08.1/";; *)

#directory "module/ocaml-4.10.0/";;

(* #directory "module/ocaml-4.11.1/";; *)

(* ----------------------------------- Analyse lexicale ----------------------------------- *)

(* Chargement du module Expression_scanner pour l'analyse lexicale *)
#load "expression_scanner.cmo";;
open Expression_scanner;;

(* Pour vérifier si le module est bien chargé *)
#show Expression_scanner;;

let test_token1 : token = Add;;            (* Expression_scanner.token = Add *)
let test_token2 : token = Variable('x');;  (* Expression_scanner.token = Variable 'x' *)
let test_token3 : token = Number(2);;      (* Expression_scanner.token = Number 2 *)

string_to_token_list "34 56 2 + x * -;";;  (* Expression_scanner.token list = [Number 34; Number 56; Number 2; Add; Variable 'x'; Multiply; Subtract; End] *)

(* --------------------- Analyse syntaxique et construction de l'arbre --------------------- *)

(* Chargement du module Stack pour manipuler des piles *)
open Stack;;

(* Création du type des arbres de syntaxe abstraite *)
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
(* Type : token list -> tree *)
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

(* Test d'expressions correctes *)
parse (string_to_token_list "34 56 2 + x * -;");;
parse (string_to_token_list "x 3 + 5 7 + + 3 4 * 1 3 + / /;");;
parse (string_to_token_list "34;");;

(* Test d'expressions incorrectes *)
parse (string_to_token_list "34 56 2 + x * -");;
parse (string_to_token_list "34 56 2 ; + x * -");;
parse (string_to_token_list "34 56 2 + + x * -");;
parse (string_to_token_list "");;
parse (string_to_token_list ";");;
parse (string_to_token_list "34");;

(* ------------------------------ Simplification sur l'arbre ------------------------------ *)  

(* Evaluation d'une sous-expression de constantes enti�res *)
let evaluate operator n1 n2 =
  match operator with
  | Plus -> Cst(n1 + n2)
  | Minus -> Cst(n1 - n2)
  | Mult -> Cst(n1 * n2)
  | Div -> Cst(n1 / n2)
;;

evaluate Plus 5 9;;
evaluate Div 8 2;;

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

eval_sub_expr Minus (Var('x')) (Var('x'));;
eval_sub_expr Plus (Cst(0)) (Var('x'));;
eval_sub_expr Div (Var('x')) (Var('y'));;
eval_sub_expr Mult(Cst(0)) (Var('x'));;

(* Evaluation d'une sous-expression *)
let rec simplification tree =
  match tree with
  | Binary(operator, Cst(n1), Cst(n2)) -> evaluate operator n1 n2
  | Binary(operator, x, y) -> (
    match (simplification x, simplification y) with
    | (Cst(n1), Cst(n2)) -> evaluate operator n1 n2
    | (simpl_x, simpl_y) -> eval_sub_expr operator simpl_x simpl_y)
  | _ -> tree
;;

simplification (Binary(Mult, Cst(3), Cst(4)));;
simplification (Binary(Mult, Cst(1), Var('x')));;
simplification (Binary(Plus, Var('y'), Cst(0)));;
simplification (Binary(Div, Var('x'), Var('x')));;
simplification (Unary(Var('x')));;

simplification (parse (string_to_token_list "34 56 2 + x * -;"));;
simplification (parse (string_to_token_list "x 3 + 5 7 + + 3 4 * 1 3 + / /;"));;

(* TODO: ajouter d'autres tests *)

(* ------------------------------ Affichage des expressions ------------------------------ *)

(* Ouvre le module Printf pour effectuer un affichage sur un terminal *)
open Printf;;
#show Printf;;

(* Test d'un printf en Ocaml *)
let _ = printf "Hello World !\n";;

let test_s = "Hello World !";;
let _ = printf "%s\n" test_s;;

(* Conversion un operateur en string *)
let string_of_operator op =
  match op with
  | Plus -> " + "
  | Minus -> " - "
  | Mult -> " * "
  | Div -> " / "
;;

string_of_operator Plus;;
string_of_operator Minus;;
string_of_operator Mult;;
string_of_operator Div;;

(* Ouverture du module Char pour utiliser la fonction 'escaped' *)
open Char;;

(* Conversion d'un arbre en string *)
let rec string_of_tree tree =
  match tree with
  | Cst(n) -> string_of_int n
  | Var(x) -> escaped x
  | Unary(exp) -> "(-"^(string_of_tree exp)^")"
  | Binary(op, x, y) ->
     match op with
     | Plus | Minus -> (string_of_tree x)^(string_of_operator op)^(string_of_tree y)
     | _ ->
        match (x, y) with
        | (Binary(_,_,_), Binary(_,_,_)) -> "("^(string_of_tree x)^")"^(string_of_operator op)^"("^(string_of_tree y)^")"
        | (Binary(_,_,_), _) -> "("^(string_of_tree x)^")"^(string_of_operator op)^(string_of_tree y)
        | (_, Binary(_,_,_)) -> (string_of_tree x)^(string_of_operator op)^"("^(string_of_tree y)^")"
                              
        | _ -> (string_of_tree x)^(string_of_operator op)^(string_of_tree y)
;;

string_of_tree (parse (string_to_token_list "34 56 2 + x * -;"));;
string_of_tree (simplification (parse (string_to_token_list "34 56 2 + x * -;")));;
string_of_tree (parse (string_to_token_list "x 3 + 5 7 + + 3 4 * 1 3 + / /;"));;
string_of_tree (simplification (parse (string_to_token_list "x 3 + 5 7 + + 3 4 * 1 3 + / /;")));;
