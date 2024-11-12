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

let rec nth list idx =
  match (idx, list) with
  | (_, []) -> raise (Failure "nth")
  | (0, x :: _) -> x
  | (i, _ :: tl) -> nth tl (i - 1)
;;

let rec list_length list count =
  match list with
  | [] -> count
  | _ :: t -> list_length t (count + 1)
;;

let length list = list_length list 0 ;;

let rec reverse list acc =
  match list with
  | [] -> []
  | h :: t -> reverse t (h :: acc)
;;

let rev list = reverse list [] ;;

let is_palindrome list =
  let reversed = rev list in
  reversed = list
;;

type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten_list list acc =
  match list with
  | [] -> acc
  | One x :: tl -> flatten_list tl (x :: acc)
  | Many y :: tl -> 
      flatten_list tl (flatten_list y acc)
;;

let flatten list = rev (flatten_list list []);;

let compress list =
  let rec aux list out_list =
    match list, out_list with
    | [], _ -> out_list (* when list is empyt *)
    | hd::tl, []  -> aux tl (hd :: out_list) (* first run *)
    | out_head :: _ , hd :: tl ->
        if out_head = hd then aux tl out_list
        else aux tl (hd :: out_list)
  in
  rev (aux list [])
;;

let pack list =
  let rec aux current acc = function
    | [] -> []
    | [x] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (a :: current) acc t
        else aux [] ((a :: current) :: acc) t
    in
    rev(aux [] [] list)
;;

let () = 
  let _ = last ["a"; "b"; "c"; "d"] in
  let _ = last [] in
  let _ = last_two ["a"; "b"; "c"; "d"] in
  let _ = last_two ["a"] in
  let _ = nth ["a"; "b"; "c"] 2 in
  let _ = length ["a"; "b"; "c"; "d"] in
  let _ = rev ["a"; "b"; "c"; "d"] in
  let _ = is_palindrome ["a"; "b"; "b"; "a"] in
  let _ = flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]] in
  let _ = print_endline "COMPRESS TIME" in
  let _ = compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] in
  let _ = pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"] in
  ()


