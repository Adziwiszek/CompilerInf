open Ast

type var_stack = var option list

let debug_print_var_stack (lst: var_stack): unit = List.iter 
   (fun x -> match x with 
             | None -> print_endline "..."
             | Some s -> print_endline s) lst

(* returns position of variable on stack *)
let place_on_stack (name: var) (env: var_stack): int = 
   let res = List.find_index 
   (fun s -> (match s with 
             | None -> false 
             | Some s -> s = name)) env 
   in (match res with 
      | None -> failwith ("unbound variable: "^name) 
      | Some n -> n)


let compile_prog (prog : prog) : vm_prog =
   let (var_list, fun_list, stmt) = prog 
in
   let make_var_stack (v_list: var list): var option list = 
      List.fold_left (fun acc v -> Some v :: acc) [] v_list
in
   let rec comp_aexp e env: cmd list * var_stack =
      match e with
      | Int n -> ([CONST n], env)
      | Var x -> let n = place_on_stack x env in ([TOP; LOAD n], env)
      | Binop (op, e1, e2) -> 
         let (e1_comp, env1) = comp_aexp e1 env in
         let (e2_comp, env2) = comp_aexp e2 (None :: env1) in
         (e1_comp @ [PUSH] @ e2_comp @ [PRIM op], (List.tl env2))
      | Call(f_name, args) -> 
         let push_args = List.fold_left 
         (fun (acc: cmd list) (arg: aexp): cmd list -> 
            let (arg_comp, _) = comp_aexp arg env in
            acc @ arg_comp @ [PUSH]) [] args in
         (push_args @ [CALL f_name] @ [LEAVE (List.length args)], env)
      (* | _ -> failwith "not implemented" *)
in
   let comp_bexp be env: cmd list * var_stack = 
      match be with
      | Bool b -> ([if b then CONST 1 else CONST 0], env)
      | Cmp(op, e1, e2) -> 
         let (e1_comp, env1) = comp_aexp e1 env in
         let (e2_comp, env2) = comp_aexp e2 (None :: env1) in
         (e1_comp @ [PUSH] @ e2_comp @ [CMP op], (List.tl env2))
      | _ -> failwith "not implemented"
in     
   let rec compile (stmt: stmt) (env: var_stack): cmd list * var_stack = 
      match stmt with 
      | Block lst -> List.fold_left 
         (fun (cmd_list, env) s -> 
            let (s_cmp, new_env) = compile s env in
            (cmd_list @ s_cmp, new_env)) ([], env) lst
      | Read x -> 
         ([TOP; PUSH; READ; STORE (place_on_stack x env)], env)
      | Write e -> 
         let (e_cmp, new_env) = comp_aexp e env in (e_cmp @ [WRITE], new_env)
      | Assgn(x, e) -> 
         let (e_cmp, new_env) = comp_aexp e env in 
         ([TOP; PUSH] @ e_cmp @ [STORE (place_on_stack x env)], new_env)
      | If(be, s1, s2) -> 
         let (be_comp, _) = comp_bexp be env in 
         let (s1_comp, _) = compile s1 env in 
         let (s2_comp, _) = compile s2 env in 
         (be_comp @ [BRANCH(s1_comp, s2_comp)], env)
      | While(b, s) -> 
         let (b_comp, env') = comp_bexp b env in 
         let (s_comp, _) = compile s env' in
         ([WHILE(b_comp, s_comp)], env') 
      | _ -> failwith "not implemented"
in 
let starting_stack = make_var_stack var_list in 
   let (res, _) = compile stmt starting_stack 
(* in let _ = debug_print_var_list sad  *)
in ([ENTER (starting_stack |> List.length)] @ res, [])
  (* 1) local variables - add to starting env local variables
     2) local functions - compile functions
     3) statement block *)
  (* compile (prog: prog) (env: var option list): vm_prog *)
  (* TODO: implement this function *)
  (* failwith "Not implemented asd" *)
