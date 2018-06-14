open Actions;;

module Block = 
  struct 
    type t = { 
      bid: int;
      stmts: Action.t array;
      calls: (string * int) array;
      lines: string array;
    }
  
  let block (bid, stmts, calls, lines) = { bid; stmts; calls; lines; }
end 