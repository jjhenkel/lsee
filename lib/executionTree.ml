
open Abstractions;;
open Expressions;;

module SS = Set.Make(String)

open BatString;;

module ExecutionTree = 
struct 
  type t = {
    mutable a: (Abstractions.t list);
    mutable pr: (int, SS.t) Hashtbl.t;
    mutable d: int;
    i: Z.t;
    mutable c: (t list);
    mutable p: bool; 
  } 
  
  let empty () = {
    a = [];
    pr = Hashtbl.create 5012;
    d = 0;
    i = Z.zero;
    c = [];
    p = true;
  }

  let create a pr d i = 
    (* let _ = print_endline ("Created abs #: " ^ (string_of_int (List.length a))) in  *)
  {
    a = a;
    pr = pr;
    d;
    i;
    c = [ ];
    p = true;
  }

  let needsrefresh t = 
    let rec worker t' = 
      if t'.p == true then
        Some (t'.d == ((List.length t'.c) + 1))
      else match t'.c with 
      | [] -> None
      | c -> List.fold_left (
          fun acc t'' -> match (worker t'') with 
          | Some v -> Some v 
          | None -> acc 
      ) None c
    in match (worker t) with 
    | Some v -> v 
    | None -> raise (Invalid_argument "Should be impossible")

  let currentpr t = 
    let rec worker t' = 
      if t'.p == true then
        Some t'.pr 
      else match t'.c with 
      | [] -> None
      | c -> List.fold_left (
          fun acc t'' -> match (worker t'') with 
          | Some v -> Some v 
          | None -> acc 
      ) None c
    in match (worker t) with 
    | Some v -> v 
    | None -> raise (Invalid_argument "Should be impossible")

  let push t c =
    let rec worker t' c' = 
      if t'.p == true then 
        let _ = c'.p <- true in
        let _ = c'.pr <- t'.pr in
        let _ = t'.c <- c' :: t'.c in 
        let _ = t'.d <- c'.d in
        let _ = t'.p <- false in () 
      else match t'.c with
      | [] -> ()
      | c -> List.iter (fun t'' -> worker t'' c') c
    in worker t c
  
  let append t c = 
    let rec worker t' c' = 
      if t'.p == true then 
        (t'.d <- c'.d ; t'.a <- t'.a @ c'.a ; 
           Hashtbl.iter (fun k a ->
             match Hashtbl.find_opt t'.pr k with 
             | Some s -> Hashtbl.replace t'.pr k (SS.union s a)
             | None -> Hashtbl.add t'.pr k a
           ) c'.pr
        )
      else match t'.c with
      | [] -> ()
      | c -> List.iter (fun t'' -> worker t'' c') c
    in worker t c

  let rec pop t =
    let rec worker t' l = 
      if t'.p == true then
        let _ = t'.p <- false 
        in let _ = l.p <- true
        (* May need to move more than one level up *) 
        in if (List.length l.c) == l.d 
          then pop t 
          else ()
      else match t'.c with
      | [] -> ()
      | c -> List.iter (fun t'' -> worker t'' t') c
    in worker t t

  let pprint tree = 
    let print_abs a d = 
      let _ = print_string ((String.make (2*d) ' ') ^ "+ ") in
      List.iteri (fun i x ->  
        if i == 0 then 
          Abstractions.pprint "" x
        else 
          Abstractions.pprint (String.make (2*(d+1)) ' ') x
        ) a 
    in let rec worker t' d = match t'.c with
    | [] -> 
      let toprint = (String.make (2*(d+1)) ' ') ^  (if t'.p == true then "(***) " ^ (string_of_int (List.length t'.c)) ^ "/" ^ (string_of_int t'.d) else (string_of_int (List.length t'.c)) ^ "/" ^ (string_of_int t'.d))
      in (print_abs t'.a d ; print_endline toprint)
    | c -> 
      let toprint = (String.make (2*(d+1)) ' ') ^  (if t'.p == true then "(***) " ^ (string_of_int (List.length t'.c)) ^ "/" ^ (string_of_int t'.d) else (string_of_int (List.length t'.c)) ^ "/" ^  (string_of_int t'.d))
      in (print_abs t'.a d ; print_endline toprint ; 
      List.iter (fun t'' -> worker t'' (d+1) ;) c)
    in worker tree 0 

  let rec final_cleanup traces = match traces with 
  | [] -> [] 
  | x :: xs -> if (BatString.exists x "__builtin_unreachable") 
      then (final_cleanup xs)
      else 
        if (BatString.is_empty (BatString.trim x)) 
          then (final_cleanup xs)
          else x :: (final_cleanup xs)

  (* Todo: try a version that only keeps N parents of context 
     when writing a trace to disk. This will remove the crazy
     buildup of shared prefixes that can be detrimental to the 
     models. 
  *)
  (* Traces where we only keep data from one parent *)
  let traces1 tree = 
    let rec worker t' trace =
      let temp = String.trim (List.fold_left
        (fun acc a -> acc ^ (Abstractions.to_string a) ^ " ")
        "" t'.a
      ) in match t'.c with
      | [] -> [ String.trim (trace ^ " " ^ temp)]
      | c -> 
        [ String.trim (trace ^ " " ^ temp) ] @ List.flatten (
          List.map (fun t'' -> worker t'' temp) c
        )
    in let res = (worker tree "")
    in let final = (final_cleanup res)
    in match final with
    | [] -> ()
    | xs -> List.iter (fun s -> print_endline (String.trim s)) xs

  let traces name tree = 
    let rec worker t' trace = 
      let temp = String.trim (List.fold_left 
        (fun acc a -> acc ^ (Abstractions.to_string a) ^ " ") 
        "" t'.a
      ) in match t'.c with
      | [] -> [ String.trim (trace ^ " " ^ temp) ]
      | c -> 
        List.flatten (List.map (fun t'' -> worker t'' (String.trim (trace ^ " " ^ temp))) c)
    in let res = (worker tree "")
    in let final = (final_cleanup res)
    in let rgx = Str.regexp "  +"
    in let clean s = (String.trim (Str.global_replace rgx " " s))
    in match final with 
    | [] -> ()
    | xs -> List.iter (fun s -> print_endline (name ^ " " ^ (clean s))) xs
    
 end