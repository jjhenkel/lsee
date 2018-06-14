
open Blocks;;
open Actions;;
open Procedures;;
open Expressions;;
open Abstractions;;
open ExecutionTree;;

module State = 
  struct 
    type t = {
      mutable etree : ExecutionTree.t;
      mutable sigma : (int, (int * Expr.t) Stack.t) Hashtbl.t;
      mutable lastblk : int;
      mutable onpath : Z.t;
      mutable depth : int;
      mutable idx : Z.t;
    }

  let get state expr = 
    (* let _ = print_endline ( "GET[" ^ string_of_int (Expr.Impl.hash expr) ^ "] (" ^ Expr.Impl.prettify expr ^ ")" ) in  *)
    match Hashtbl.find_opt state.sigma expr.Hashcons.hkey with 
    | Some s ->
        let rec loop () = 
            if Stack.is_empty s then 
                expr
            else 
                let (d, x) = Stack.top s in 
                if state.depth < d then 
                    let _ = Stack.pop s in loop () 
                else
                    (* let _ = print_endline (" --(HIT)> " ^ (Expr.prettify x)) in   *)
                    x
        in loop ()    
    | None ->
        (* let _ = print_endline (" --(MISS)> None ") in  *)
        expr

  let empty () = { 
    etree = ExecutionTree.empty ();
    sigma = Hashtbl.create 123456;
    lastblk = -1;
    onpath = Z.zero;
    depth = 0;
    idx = Z.zero;
  }
  
  let executestmt i curr stmt state = 
    (* let _ = print_endline curr.Cfg.Vert.blk.Block.lines.(i) in *)
    (* let _ = Action.prettify stmt |> print_endline in  *)
    let res = match stmt.Hashcons.node with 
    | Action.Impl.Nopped -> stmt
    | Action.Impl.Started -> stmt
    | Action.Impl.Finished -> stmt
    | Action.Impl.Debugged s -> stmt
    | Action.Impl.Predicted s -> stmt
    | Action.Impl.Unsupported s -> stmt
    | Action.Impl.Called e -> 
        Action.call (Expr.substitute e (get state))  
    | Action.Impl.Assumed (idx, e) -> 
        if state.lastblk == idx then 
          Action.assume (idx, (Expr.substitute e (get state)))
        else 
          Action.nop
    | Action.Impl.Observed e -> 
        Action.observe (Expr.substitute e (get state))
    | Action.Impl.Returned e -> 
        Action.return (Expr.substitute e (get state))
    | Action.Impl.Assigned (x, y) -> 
        let expr = Expr.substitute y (get state)
        in let temp =  Hashtbl.find_opt state.sigma x.Hashcons.hkey
        (* in let _ = print_endline ("STORE[" ^ (string_of_int x.Hashcons.hkey) ^ "] = ( " ^ (string_of_int state.depth) ^ ", " ^ (Expr.prettify expr) ^ ")")  *)
        in let _ = match temp with 
        | Some s ->
            if Stack.is_empty s then 
                Stack.push (state.depth, expr) s
            else
                let (d, x) = Stack.top s in 
                if d == state.depth then
                    let _ = Stack.pop s in 
                    Stack.push (state.depth, expr) s
                else 
                    Stack.push (state.depth, expr) s
        | None ->
          let s = Stack.create () in 
          let _ = Stack.push (state.depth, expr) s in
          Hashtbl.add state.sigma x.Hashcons.hkey s
        in Action.assign (x, expr)
    (* in let _ = Action.prettify res |> print_endline in let _ = print_endline "" *)
    in res

  (* let rec collapse_dup_aps arr = match arr with 
  | [] -> []
  | x :: [] -> [x] 
  | x :: y :: tl -> (match (x, y) with 
    | Abstractions.AccessPathLoad s1, Abstractions.AccessPathLoad s2 ->
        if s1 = s2 
            then collapse_dup_aps (y :: tl)
            else x :: collapse_dup_aps (y :: tl)
    | Abstractions.AccessPathStore s1, Abstractions.AccessPathStore s2 ->
        if s1 = s2 
            then collapse_dup_aps (y :: tl)
            else x :: collapse_dup_aps (y :: tl)
    | Abstractions.AccessPathSensitive s1, Abstractions.AccessPathSensitive s2 ->
        if s1 = s2 
            then collapse_dup_aps (y :: tl)
            else x :: collapse_dup_aps (y :: tl)
    | _ -> x :: collapse_dup_aps (y :: tl)
  ) *)

  let execute curr state = 
    let (_, res) = Array.fold_left 
      (fun (i, acc) stmt -> (i + 1, (executestmt i curr stmt state) :: acc))
      (0, [])
      curr.Cfg.Vert.blk.Block.stmts
    in let _ = state.idx <- Z.succ state.idx
    in Abstractions.abstract res (ExecutionTree.currentpr state.etree)

end
