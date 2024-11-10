let rec last list =
  match list with
  | [] -> None
  | [x] -> Some x
  | _ :: tl  -> last tl
;;

let rec last_two list =
  match list with
  | [] -> None
  | [_] -> None
  | [x ; y] -> Some (x,y)
  | _ :: tl -> last_two tl
;;

let () = 
  let _ = last ["a"; "b"; "c"; "d"] in
  let _ = last [] in
  let _ = last_two ["a"; "b"; "c"; "d"] in
  let _ = last_two ["a"] in
  ()
