(* Décommenter la commande directory selon votre verison d'ocaml *)

(* #directory "module/ocaml-4.02.1+ocp1/";; *)

(* #directory "module/ocaml-4.02.3/";; *)

(* #directory "module/ocaml-4.05.0/";; *)

#directory "module/ocaml-4.08.1/";;

(* #directory "module/ocaml-4.10.0/";; *)

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
  in
  parse_aux token_list
;;
  
parse (string_to_token_list "34 56 2 + x * -;");;
