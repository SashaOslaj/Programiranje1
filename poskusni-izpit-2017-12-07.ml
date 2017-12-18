(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

(* 1.1) Definirajte funkcijo, ki vzame dve celi števili ter vrne njuno vsoto.
   Primer: /sestej 2 3 = 5/ *)
let sestej x y = x + y 

(* 1.2) Definirajte funkcijo, ki svojemu argumentu prišteje 3.
   Primer: /pristej_tri 10 = 13/ *)
let pristej_tri x = x + 3

(* 1.3) Definirajte funkcijo, ki vsem elementom seznama prišteje 5.
   Primer: /vsem_pristej_pet [1; 2] = [6; 7]/ *)
let rec vsem_pristej_pet l = 
  match l with 
  | [] -> []
  | x :: xs -> (x + 5) :: vsem_pristej_pet xs

(* 1.4) Definirajte funkcijo, ki vrne zadnjo komponento nabora s tremi elementi.
   Primer: /tretji (1, "horse", [None]) = [None]/ *)
let tretji = function (x, y, z) -> z
  

(* 1.5) Definirajte funkcijo, ki vzame dve funkciji ter vrne njun kompozitum.
   Primer: /kompozitum succ string_of_int 5 = "6"/ *)
let kompozitum f g x = g (f x) 


(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

(* 2.1) Rožno drevo je drevo, v katerem ima vsak koren poljubno mnogo otrok,
   ki so zopet rožna drevesa. Rožna drevesa predstavimo s parametričnim
   tipom /'a drevo/ z enim konstruktorjem, ki sprejme:
   - vrednost (koren) tipa /'a/ in
   - seznam (gozd) dreves tipa /'a drevo/. *)
type 'a drevo = Empty | Node of 'a drevo * 'a * 'a drevo

let leaf x = Node(Empty, x, Empty)

let test_tree1 = Node( Node(leaf 0, 2, Empty), 5, Node(leaf 6, 7, leaf 11))

let test_tree2 = Node( Node(leaf 0, (-1), Empty), 5, Node(leaf 6, 7, leaf 11))

(* 2.2) Napišite funkcijo, ki vrne koren danega rožnega drevesa. *)
let koren drevo = 
  match drevo with
  | Empty -> Empty
  | Node(l, x, r) -> x 

(* 2.3) Napišite funkcijo, ki preveri, ali drevo celih števil vsebuje kakšno negativno število. *)
let kaksno_negativno drevo = 
  let rec naredi_seznam drevo = 
    match drevo with 
    | Empty -> []
    | Node(l, x, r) -> (naredi_seznam l) @ [x] @ (naredi_seznam r) @ []  
  in
  let rec preveri_negativno = function
    | [] -> false
    | hd :: tl -> 
	  if hd >= 0 
	  then preveri_negativno tl
	  else true
  in 
  drevo |> naredi_seznam |> preveri_negativno
  

(* 2.4) Sestavite funkcijo, ki sprejme naravno število ter sestavi (poljubno)
   drevo, ki ima toliko otrok.
   Namig: napišite pomožno funkcijo, ki ustvari poljuben seznam dane dolžine. *)

   
let drevo_z_veliko_otroci = failwith "dopolni me"

(* 2.5) Sestavite funkcijo, ki izračuna število vseh vozlišč v drevesu.
   Če želite vse točke, mora biti funkcija repno rekurzivna.

   Opomba: kot ste videli na vajah, nekatere funkcije iz modula List,
   na primer List.map, niso repno rekurzivne, zato se jim raje
   izognite. *)
let velikost = failwith "dopolni me"
