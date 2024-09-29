(* All exercises are solved in a tail recursive manner *)

(* Beginner *)

let rec last list =
  match list with [] -> None | [ x ] -> Some x | _ :: xs -> last xs

let rec last_two = function
  | [] | [ _ ] -> None
  | [ x1; x2 ] -> Some (x1, x2)
  | _ :: xs -> last_two xs

let rec nth list n =
  match list with [] -> None | x :: xs -> if n = 0 then x else nth xs (n - 1)

let length list =
  let rec aux acc = function [] -> acc | _ :: xs -> aux (acc + 1) xs in
  aux 0 list

let rev list =
  let rec aux acc = function [] -> acc | x :: xs -> aux (x :: acc) xs in
  aux [] list

let is_palindrome list = list = rev list

let encode list =
  let rec aux acc count = function
    | [] -> acc
    | [ x ] -> (count + 1, x) :: acc
    | x :: (xs :: _ as t) ->
        if x = xs then aux acc (count + 1) t
        else aux ((count + 1, x) :: acc) 0 t
  in
  aux [] 0 list

type 'a rle = One of 'a | Many of int * 'a

(* unsure *)
let encode_mod list =
  let tupler count element =
    if count = 1 then One element else Many (count, element)
  in
  let rec aux acc count = function
    | [] -> acc
    | [ x ] -> tupler (count + 1) x :: acc
    | x :: (xs :: _ as t) ->
        if x = xs then aux acc (count + 1) t
        else aux (tupler (count + 1) x :: acc) 0 t
  in
  List.rev (aux [] 0 list)

(* TODO: w/ List.map *)
let duplicate list =
  let rec aux acc = function [] -> acc | x :: xs -> aux (x :: x :: acc) xs in
  List.rev (aux [] list)

(*
   return a tuple of 2 lists
   form: ([x1,..., xn], [y1,..., yn])
   1. tuplify the lists
*)
let split list n =
  let rec aux acc n = function
    | [] -> (acc, [])
    | x :: xs as ll -> if n = 0 then (acc, ll) else aux (x :: acc) (n - 1) xs
  in
  aux [] n list

(* assuming @ is tail recursive *)
let remove_at k list =
  let rec aux acc k = function
    | [] -> acc
    | x :: xs -> if k = 0 then acc @ xs else aux (x :: acc) (k - 1) xs
  in
  List.rev (aux [] k list)

let insert_at elem index list =
  let rec aux acc elem index = function
    | [] -> List.rev (elem :: acc)
    | x :: xs ->
        if index = 0 then List.rev_append acc (elem :: x :: xs)
        else aux (x :: acc) elem (index - 1) xs
  in
  aux [] elem index list

let range fst snd =
  let rec aux acc high low =
    let abs_i = abs (high - low + 1) in
    match abs_i with
    | 0 -> acc
    | _ when fst <= snd -> aux (high :: acc) (high - 1) low
    | _ -> aux (low :: acc) high (low + 1)
  in
  aux [] (max fst snd) (min fst snd)

(*
     lotto_select 6 49;;
   - : int list = [20; 28; 45; 16; 24; 38]
*)

let lotto_select n set = failwith "TODO: later. w/ selection algorithm"
let permutation list = failwith "TODO: later. w/ selection algorithm"
let coprime n1 n2 = failwith "todo w/ gcd"

(* Intermediate *)

(* changed type definition so that it wouldn't interfere with future rle usage *)
type 'a node = OneNode of 'a | ManyNode of 'a node list

let flatten node =
  let rec aux acc = function
    | [] -> acc
    | OneNode x :: xs -> aux (x :: acc) xs
    | ManyNode x :: xs -> aux (aux acc x) xs
  in
  List.rev (aux [] node)

(* removes the consecutive duplicates *)
let compress list =
  let rec aux acc = function
    | x1 :: (x2 :: _ as t) -> if x1 = x2 then aux acc t else aux (x1 :: acc) t
    | xs -> List.rev_append acc xs
  in
  aux [] list

(* Helper for future functions *)
let rec contains list' v =
  match list' with
  | [] -> false
  | x :: xs -> if x = v then true else contains xs v

(* removes all duplicates from the list,
    not just the consecutives *)
let compress_all list =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> if contains acc x then aux acc xs else aux (x :: acc) list
  in
  List.rev (aux [] list)

let pack list =
  let rec aux acc current = function
    | [] -> []
    | [ x ] -> (x :: current) :: acc
    | x1 :: (x2 :: _ as t) ->
        if x1 = x2 then aux acc (x1 :: current) t
        else aux ((x1 :: current) :: acc) [] t
  in
  List.rev (aux [] [] list)

let encode_direct list =
  let rle count value =
    if count = 0 then One value else Many (count + 1, value)
  in
  let rec aux acc count = function
    | [] -> acc
    | [ x ] -> rle count x :: acc
    | x :: (xs :: _ as tail) ->
        if x = xs then aux acc (count + 1) tail
        else aux (rle count x :: acc) 0 tail
  in
  List.rev (aux [] 0 list)

(* not tail recursive but simpler with the List.module *)
let replicate_with_map list n =
  List.flatten (List.map (fun x -> List.init n (fun _ -> x)) list)

(*
   tail recursive, w/ List.fold_left but kinda confusing
*)
let replicate_with_foldl list n =
  let rec aux elem acc count =
    if count = 0 then acc else aux elem (elem :: acc) (count - 1)
  in
  List.fold_left (fun acc x -> aux x acc n) [] list |> List.rev

(* "manual" implementation *)
let replicate list n =
  (* replicate elements n times here, then go back to aux *)
  let rec replicate_element element acc count =
    if count = 0 then acc
    else replicate_element element (element :: acc) (count - 1)
  in
  let rec aux acc = function
    | [] -> List.rev acc
    | x :: xs -> aux (replicate_element x acc n) xs
    (* send the value with n times to replicate_element and repeat for all elements*)
  in
  aux [] list

(*
  not tail recursive, w/ List module
*)
let drop list n =
  List.mapi (fun i x -> (i + 1, x)) list
  |> List.filter (fun (i, _) -> i mod n <> 0)
  |> List.map snd

(* not the prettiest implementation but it works *)
let drop list n =
  let rec aux acc count = function
    | [] -> List.rev acc
    | [ x ] -> aux (x :: acc) (count - 1) []
    | x :: xs ->
        if count = 1 then aux acc n xs else aux (x :: acc) (count - 1) xs
  in
  aux [] n list

(* Helper function to drop the first i elements of the list *)
let drop_first_i i list =
  let rec aux i list =
    match list with
    | [] -> []
    | _ :: xs when i > 0 -> aux (i - 1) xs
    | _ -> list
  in
  aux i list

(* assuming i <= k *)
let slice list i k =
  let count = k - i + 1 in
  let rec aux acc count = function
    | [] -> List.rev acc
    | x :: xs ->
        if count = i then aux acc count [] else aux (x :: acc) (count - 1) xs
  in
  aux [] (count + i) (drop_first_i i list)

(* official solution *)
let rec fold_until f acc n = function
  | [] -> (acc, [])
  | h :: t as l -> if n = 0 then (acc, l) else fold_until f (f acc h) (n - 1) t

let slice list i k =
  let _, list = fold_until (fun _ _ -> []) [] i list in
  let taken, _ = fold_until (fun acc h -> h :: acc) [] (k - i + 1) list in
  List.rev taken

(* using previous functions and List logic *)
let rotate_with_drop list n =
  let list' = drop_first_i n list in
  let dropped = drop_first_i (List.length list - n) (List.rev list) in
  List.append list' (List.rev dropped)

(* "manual" implementation *)
let rotate list n = failwith "TODO later"

(* unrelated to what I'm learning, will take a look later *)
let rand_select list n =
  let rand = Random.init (Random.int Int.max_int) in
  let rec aux acc n = function [] -> acc | x :: xs -> failwith "todo" in
  aux [] n list

(* wrong implementation but put my heart and soul into it, literally *)
let extract k list =
  let rec aux acc k' = function
    | [] -> [ List.rev acc ]
    | [ _ ] -> aux acc k' []
    | x :: xs ->
        let with_x = aux (x :: acc) (k' - 1) xs in
        let without_x = aux acc k' xs in
        with_x @ without_x
  in
  aux [] k list

let extract k list =
  let rec aux acc k list =
    match (k, list) with
    | 0, _ -> [ List.rev acc ]
    | _, [] -> []
    | k, x :: xs ->
        let with_x = aux (x :: acc) (k - 1) xs in
        let without_x = aux acc k xs in
        with_x @ without_x
  in
  aux [] k list

let group (list : 'a list) (group_num : int list) = failwith "todo"
