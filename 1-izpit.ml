let rec izpisi_vsa_stevila l =
  match l with
  | [] -> ()
  | hd :: tl -> print_int hd ; izpisi_vsa_stevila tl
  
let map2_opt f l1 l2 = 
  if List.length l1 = List.length l2
  then
    let rec sestej l1 l2 = 
	  match (l1, l2) with
	  | ([], _) -> []
	  | (_, []) -> []
	  | ([x], [y]) -> [f x y]
	  | (x::xs, y::ys) -> (f x y) :: sestej xs ys
	in 
	Some (sestej l1 l2)
  else None
  
type filter_tree = Node of filter_tree * int * filter_tree 
				   | Box of int list

let filter_tree1 = Node(Node((Box [1]), 5, (Box [])), 10, Node((Box []), 15, (Box [19; 20]))) 

let rec vstavi k = function
  | Box [] -> Box [k]
  | Box [x] -> Box (k::[x])
  | Box (hd::tl) -> Box (k::hd::tl)
  | Node(l, n, r)-> 
    if k <= n then
	  Node(vstavi k l, n, r)
	else 
	  Node(l, n, vstavi k r)
	  
let rec vstavi_seznam l tree = 
  List.fold_right vstavi l tree
  
type vektor = int * int
type matrika = int * int * int * int

module type Linearna = sig
    type t 
	
	val id          : t 
	val uporabi     : t -> vektor -> vektor 
	val iz_matrike  : matrika -> t 
	val iz_funkcije : (vektor -> vektor) -> t 
	val kompozitum  : t -> t -> t 
end
	
module Matrika : Linearna = struct
    type t = matrika
	
	let id = (1, 0, 0, 1)
	let uporabi m v = 
	  match (m, v) with
	  | (id, v) -> v 
	  | ((a, b, c, d), (x, y)) -> (a*x + b*y, c*x + d*y)
	let iz_matrike m = m 
	let iz_funkcije f =
	  let (a, c) = f (1, 0) in
	  let (b, d) = f (0, 1) in
	  (a, b, c, d)
	let kompozitum m1 m2 = 
	  match (m1, m2) with
	  |(id, m2) -> m2
	  |(m1, id) -> m1
	  |((a1, b1, c1, d1), (a2, b2, c2, d2)) -> (a1*a2 + b1*c2, a1*b2 + b1*d2, c1*a2 + d1*c2, c1*b2 + d1*d2)
end

module Funkcija : Linearna = struct
    type t = vektor -> vektor
	
	let id = (fun x -> x)
	let uporabi f v = f v 
	let iz_matrike (a, b, c, d) = fun (x, y) -> (a*x + b*y, c*x + d*y)
	let iz_funkcije f = f 
	let kompozitum f1 f2 = fun x -> f1 (f2 x) 
end
	

	  