
module Util = 
  struct 
    let time f x =
      let start = Unix.gettimeofday ()
      in let res = f x
      in let stop = Unix.gettimeofday ()
      in let () = Printf.printf "Execution time: %fs\n%!" (stop -. start)
      in
         res
end