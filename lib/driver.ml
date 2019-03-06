open Z3
open Z3.Symbol
open Z3.Sort
open Z3.Expr
open Z3.Boolean
open Z3.FuncDecl
open Z3.Goal
open Z3.Tactic
open Z3.Tactic.ApplyResult
open Z3.Probe
open Z3.Solver
open Z3.Arithmetic
open Z3.Arithmetic.Integer
open Z3.Arithmetic.Real
open Z3.BitVector


open Utils;;
open State;;
open Blocks;;
open Actions;;
open GccTypes;;
open Procedures;;
open Expressions;;
open Abstractions;;
open ExecutionTree;;

module Driver = 
  struct 
    (*let execute f = (Util.time Proc.printpaths) (f, Z.of_string "1000")*)
    let execute f = 
    if Z.gt f.Proc.cfg.Cfg.paths Z.zero then 
      (Cfg.traversepaths) (
          Z.of_string "5000",
          (* Performed at each step of execution *)
          (fun (curr, state) ->
            let (pr, abs) = (State.execute curr state) in 
            state.State.lastblk <- curr.Cfg.Vert.blk.Block.bid ; 
            ExecutionTree.append state.State.etree (
              ExecutionTree.create abs pr (Array.length curr.Cfg.Vert.succ) 
              state.State.idx
            )
          ),
          (* Performed on pop *)
          (fun (curr, state) -> 
            match curr with 
            | Some _ -> 
              (* print_endline ("POP") ; *)
              (* print_newline () ;  *)
              state.State.depth <- state.State.depth - 1 ;
              state.State.onpath <- Z.succ state.State.onpath ;
              (* print_endline ("On path :: " ^ (Z.to_string state.State.onpath) ^ "/" ^ (Z.to_string f.Proc.cfg.Cfg.paths)) ; *)
              (* print_endline ("Pre-pop: ") ; *)
              (* ExecutionTree.pprint state.State.etree ; *)
              ExecutionTree.pop state.State.etree ; 
              (* print_endline ("Post-pop: ") ; *)
              (* ExecutionTree.pprint state.State.etree ; *)
              if state.State.onpath == (Z.of_string "5000") then (
                (* ExecutionTree.pprint state.State.etree ;  *)
                ExecutionTree.traces f.Proc.name state.State.etree
              ) else ()
            | None -> ExecutionTree.traces f.Proc.name state.State.etree
          ),
          (* Performed on push *)
          (fun (curr, state) -> 
            if curr.Cfg.Vert.pushes == (Array.length curr.Cfg.Vert.succ) - 2 then (
            (* print_endline ("PUSH :: " ^ (string_of_int curr.Cfg.Vert.pushes)) ; *)
            (* print_endline ("CHILDREN :: " ^ (string_of_int (Array.length curr.Cfg.Vert.succ))) ; *)
            state.State.depth <- state.State.depth + 1 ;
            ExecutionTree.push state.State.etree (
              ExecutionTree.create [] (Hashtbl.create 5012) (Array.length curr.Cfg.Vert.succ) state.State.idx
            )
            ) else ()
          ),
          (* Performed to refresh state (directly after pop) *)
          (fun (curr, state) -> 
            (* print_endline ("PUSH 2 :: " ^ Z.to_string state.State.idx) ; *)
            (* print_endline ("CHILDREN :: " ^ (string_of_int (Array.length curr.Cfg.Vert.succ))) ; *)
            let _ = State.execute curr state in 
            state.State.lastblk <- curr.Cfg.Vert.blk.Block.bid ;
            ExecutionTree.push state.State.etree (
              ExecutionTree.create [] (Hashtbl.create 5012) (Array.length curr.Cfg.Vert.succ) state.State.idx
            )
          ),
          (* Initializer *)
          (fun () -> State.empty ()),
          f.Proc.cfg
        )

  end