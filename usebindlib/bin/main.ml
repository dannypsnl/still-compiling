(* Modify https://github.com/rlepigre/ocaml-bindlib/blob/master/examples/basic.ml *)
open Bindlib

type tm =
  | Var of tm var
  | Abs of (tm, tm) binder
  | App of tm * tm

let rec eval : tm -> tm = function
  | App (f, a) -> begin
    match eval f with
    | Abs binder -> eval (subst binder a)
    | _ -> App (f, a)
  end
  | t -> t
;;

let mkfree : tm var -> tm = fun x -> Var x

(* We need to provide a function that converts our term to boxed-term *)
let rec box_term : tm -> tm box =
  fun t ->
  match t with
  | Var x -> var x
  | Abs b ->
    (* box_binder is provided by bindlib *)
    abs_raw (box_binder box_term b)
  | App (t, u) -> app (box_term t) (box_term u)

and var x =
  (* box_var is provide by bindlib *)
  box_var x

and abs_raw : (tm, tm) binder box -> tm box = fun b -> box_apply (fun b -> Abs b) b
and abs : tm var -> tm box -> tm box = fun x t -> abs_raw (bind_var x t)

and app : tm box -> tm box -> tm box =
  fun t u ->
  (* box_apply2 is provided by bindlib, a functorial way to operate boxed-term *)
  box_apply2 (fun t u -> App (t, u)) t u
;;

let rec to_string : ctxt -> tm -> string =
  fun ctxt t ->
  match t with
  | Var x -> name_of x
  | Abs b ->
    let x, t, ctxt = unbind_in ctxt b in
    "λ" ^ name_of x ^ "." ^ to_string ctxt t
  | App (t, u) -> "(" ^ to_string ctxt t ^ ") " ^ to_string ctxt u
;;

let to_string : tm -> string = fun t -> to_string (free_vars (box_term t)) t

let id : tm =
  let x = new_var mkfree "x" in
  unbox (abs x (var x))
;;

let () =
  print_endline (to_string id);
  let y = new_var mkfree "y" in
  print_endline (to_string (eval @@ unbox (app (box_term id) (var y))))
;;
