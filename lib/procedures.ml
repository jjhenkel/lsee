open Blocks;;
open Actions;;
open GccTypes;;
open Expressions;;

module Cfg = 
  struct
    module Edge = 
      struct type t = {
        idx: int;
        uid: string;
        blk: Block.t;
        low: Z.t;
        high: Z.t;
      }
    end
    
    module Vert = 
      struct type t = {
        uid: string;
        blk: Block.t;
        succ: Edge.t array;
        mutable pushes: int;
      }
    end 

    type t = {
      blocks : int;
      paths : Z.t;
      adj : Vert.t array;
    } 
  
  let cfg (blocks, paths, adj) = { blocks; paths; adj; }
  let vert (uid, blk, succ) = { Vert.uid; blk; succ; pushes = (Array.length succ) - 1; }
  let edge (idx, uid, blk, low, high) = { Edge.idx; uid; blk; low; high; }

  let rec edgebypath (arr, path) = 
    Array.fold_left (fun x y -> match x with 
    | Some _ as x -> x
    | None -> 
        if Z.leq y.Edge.low path && Z.leq path y.Edge.high
          then Some y else None
    ) None arr
  
  let traversepaths (upto, visit, visitpath, visitpush, refresh, initstate, cfg) = 
    let rec dfs (curr, last, path, bl, state, skipvisit) =
      (* We only work on paths up to the threshold (upto) *)
      if path > upto then () else (
        (* let _ = print_endline ("Visit " ^ curr.Vert.uid) in *)
        (* Visit this vertex (if we should) *)
        let _ = if skipvisit then refresh (curr, state) else visit (curr, state) in
        match curr.Vert.succ with 
        (* The way we've set things up in our generation step is such 
           that the ONLY vert with no successors is the exit. We've
           reached the end of a path! Increment our counter and continue
           the depth first exploration
        *)
        | [| |] -> (
          (* Make sure we can keep going *)
          if Stack.is_empty last then visitpath (None, state) else (
            (* Pop off the stack, we've reached an exit *)
            let (newcurr, newbl) = Stack.pop last
            (* Visit this path *)
            in let _ = visitpath (Some newcurr, state)
            (* Continue our search from this new starting point *)
            in dfs (newcurr, last, Z.succ path, newbl, state, true)
          )
        )
        (* No choice, so we just continue on *)
        | [| e |] -> (
          dfs (cfg.adj.(e.Edge.idx), last, path, Z.sub bl e.Edge.low, state, false)
        )
        (* In this case we have a branch or switch *)
        | _ as arr -> (
          (* If we are coming in 'clean' refersh push count *)
          let _ = if skipvisit then () 
            else 
              curr.Vert.pushes <- (Array.length curr.Vert.succ) - 1
          (* Grab the correct edge based on the BL path info *)
          in let correctedge = match edgebypath (arr, bl) with 
            | Some x -> x 
            (* This shouldn't happen *)
            | None -> raise (Invalid_argument "Sanity check failed.")
          (* Push our decision point onto the stack if we haven't 
             already visited it the appropriate number of times.
          *)
          in let _ = if curr.Vert.pushes > 0 then
            let _ = curr.Vert.pushes <- curr.Vert.pushes - 1 
            in let _ = visitpush (curr, state) 
            in Stack.push (curr, Z.succ correctedge.Edge.high) last 
            else () 
          (* Find that vertex in our adjacency list *)
          in let newcurr = cfg.adj.(correctedge.Edge.idx) 
          (* Continue our depth first exploration *)
          in dfs (newcurr, last, path, Z.sub bl correctedge.Edge.low, state, false)
        )
      )
    (* Start our search from entry (always 0 in adj list) *)
    in dfs (cfg.adj.(0), Stack.create (), Z.zero, Z.zero, initstate (), false)
end

module Proc = 
  struct 
    type t = {
      name : string;
      fid : int;
      workdir : string;
      sourcefile: string;
      basename: string;
      cfg: Cfg.t;
    }
  
  let proc (name, fid, workdir, sourcefile, basename, cfg) = {
    name; fid; workdir; sourcefile; basename; cfg; 
  }

  let printpaths (proc, upto) = 
    Cfg.traversepaths (
      upto, 
      (fun (x,_) -> print_string (x.Cfg.Vert.uid ^ " ")), 
      (fun (_,_) -> print_newline ()),
      (fun (_,_) -> ()),
      (fun (_,_) -> ()),
      (fun () -> ()),
      proc.cfg
    ) ; print_endline "" 
end 
