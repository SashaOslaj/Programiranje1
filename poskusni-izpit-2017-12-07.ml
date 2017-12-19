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
type 'a drevo = Empty | Node of 'a * 'a drevo list 

let rose_tree1 = Node (1, [Empty; Node (1, [Empty])])
let rose_tree2 = Node (2, [Empty; Node (-2, [Empty]); Node (2, [Empty])])

(* 2.2) Napišite funkcijo, ki vrne koren danega rožnega drevesa. *)
let koren = function
  | Empty -> None
  | Node(x, _) -> Some x 

(* 2.3) Napišite funkcijo, ki preveri, ali drevo celih števil vsebuje kakšno negativno število. *)
let rec kaksno_negativno drevo = 
  match drevo with
  | Empty -> false
  | Node(x, poddrevo) -> 
    if x >= 0
	then let rec kaksno_negativno_poddrevo = function
	       | [] -> false
		   | hd::tl -> kaksno_negativno hd || (kaksno_negativno_poddrevo tl)
         in kaksno_negativno_poddrevo poddrevo
	else true
  

(* 2.4) Sestavite funkcijo, ki sprejme naravno število ter sestavi (poljubno)
   drevo, ki ima toliko otrok.
   Namig: napišite pomožno funkcijo, ki ustvari poljuben seznam dane dolžine. *)

   
let rec seznam n = 
  if n = 0 
    then []
  else
    (Node (n, [Empty]))::(seznam (n-1))

let rec drevo_z_veliko_otroci n =
  if n = 0
    then Empty
  else
    Node (n, seznam n)

(* 2.5) Sestavite funkcijo, ki izračuna število vseh vozlišč v drevesu.
   Če želite vse točke, mora biti funkcija repno rekurzivna.

   Opomba: kot ste videli na vajah, nekatere funkcije iz modula List,
   na primer List.map, niso repno rekurzivne, zato se jim raje
   izognite. *)
let rec velikost t = 
  match t with
    | Empty -> 0
    | Node (a, trees) ->
      let rec aux_velikost = function
       | [Empty]|[] -> 0
       | hd::tl -> (velikost hd) + (aux_velikost tl)
      in
      1 + aux_velikost trees
