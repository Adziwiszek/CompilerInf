open Ast

type var_stack = var option list

let debug_print_var_list (lst: var_stack): unit = List.iter 
   (fun x -> match x with 
             | None -> print_endline "..."
             | Some s -> print_endline s) lst

(* returns position of variable on stack *)
let place_on_stack (name: var) (env: var_stack): int = 
   let res = List.find_index (fun s -> 
   (match s with | None -> false | Some s -> s = name)) env 
   in (match res with | None -> failwith ("unbound variable: "^name) | Some n -> n)


let compile_prog (prog : prog) : vm_prog =
   let (var_list, _, stmt) = prog in
   (* let rec make_var_stack (v_list: var list) (acc: var option list): var option list =
      match v_list with 
      | [] -> acc 
      | v :: v_list' -> make_var_stack v_list' (Some v :: acc) *)
   let make_var_stack (v_list: var list): var option list = 
      List.fold_left (fun acc v -> Some v :: acc) [] v_list
in
   let rec comp_aexp e env: cmd list * var_stack =
      match e with
      | Int n -> ([CONST n], env)
      | Var x -> let n = place_on_stack x env in ([TOP; LOAD n], env)
      | Binop (op, e1, e2) -> 
         let (e1_comp, _) = comp_aexp e1 env in
         let (e2_comp, env2) = comp_aexp e2 (None :: env) in
         (e1_comp @ [PUSH] @ e2_comp @ [PRIM op], env2)
      | _ -> failwith "not implemented"
in     
   let rec compile (stmt: stmt) (env: var_stack): cmd list * var_stack = 
      match stmt with 
      | Block lst -> List.fold_left 
         (fun (cmd_list, env) s -> 
            let (s_cmp, new_env) = compile s env in
            (cmd_list @ s_cmp, new_env)) ([], env) lst
      | Read _ -> (*TODO: add looking if var is declared *)([READ; PUSH], env)
      | Write e -> 
         let (e_cmp, new_env) = comp_aexp e env in (e_cmp @ [WRITE], new_env)
                  
      | _ -> failwith "not implemented"
in 
   let (res, sad) = compile stmt (make_var_stack var_list) 
in let _ = debug_print_var_list sad 
in (res, [])
  (* 1) local variables - add to starting env local variables
     2) local functions - compile functions
     3) statement block *)
  (* compile (prog: prog) (env: var option list): vm_prog *)
  (* TODO: implement this function *)
  (* failwith "Not implemented asd" *)
