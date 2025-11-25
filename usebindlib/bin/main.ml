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

(* In this case mkfree is straightforward *)
let mkfree (x : tm var) : tm = Var x
let var' (name : string) : tm var = new_var mkfree name

(* We need to provide a function that wrap our term *)
let rec box_term (t : tm) : tm box =
  match t with
  | Var x -> var x
  | Abs b ->
    (* box_binder is provided by bindlib *)
    abs_raw (box_binder box_term b)
  | App (t, u) -> app (box_term t) (box_term u)

and var x =
  (* box_var is provide by bindlib *)
  box_var x

and abs_raw (b : (tm, tm) binder box) : tm box = box_apply (fun b -> Abs b) b
and abs (x : tm var) (t : tm box) : tm box = abs_raw (bind_var x t)

(* box_apply2 is provided by bindlib, a functorial way to operate boxed-term *)
and app (t : tm box) (u : tm box) : tm box = box_apply2 (fun t u -> App (t, u)) t u

(* A recursive definition to print thing out *)
let rec to_string (ctxt : ctxt) (t : tm) : string =
  match t with
  | Var x -> name_of x
  | Abs b ->
    let x, t, ctxt = unbind_in ctxt b in
    "λ" ^ name_of x ^ "." ^ to_string ctxt t
  | App (t, u) -> "(" ^ to_string ctxt t ^ ") " ^ to_string ctxt u
;;

(* free_vars compute out a context for a boxed term *)
let to_string (t : tm) : string = to_string (free_vars (box_term t)) t

(* λx.x *)
let id : tm =
  let x = var' "x" in
  unbox (abs x (var x))
;;

(* λx.λy.x *)
let fst : tm =
  let x = var' "x" in
  let y = var' "y" in
  unbox (abs x (abs y (var x)))
;;

(* λx.(x) x) (boxed) *)
let delta' : tm box =
  let x = var' "x" in
  abs x (app (var x) (var x))
;;

(* (λx.(x) x) λx.(x) x *)
let omega : tm = unbox (app delta' delta')

(* λx.(x) x) *)
let delta : tm = unbox delta'

let () =
  print_endline (to_string id);
  print_endline (to_string fst);
  print_endline (to_string omega);
  print_endline (to_string delta);
  let y = var' "y" in
  print_endline (to_string (eval @@ unbox (app (box_term id) (var y))))
;;
