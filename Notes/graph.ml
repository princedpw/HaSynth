(* LIBRARY OF MAP IMPLEMENTATIONS *)

module type MAP =
  sig
    type ('a,'b) t
    val empty : unit -> ('a,'b) t 
    val get : ('a,'b) t -> 'a -> 'b  (* raises Not_found if not found *)
    val insert : ('a,'b) t -> 'a -> 'b -> unit  (* overwrites *)
    val fold : (('a * 'b) -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
  end
 
module ListMap : MAP =
  struct
    type ('a,'b) t = ('a * 'b) list ref
    let empty () = ref []
    let get m k = let (_,v) = List.find (fun (k',v) -> k = k') (!m) in v
    let insert m k v = 
      let rec ins l =
	match l with
	    (k',v')::rest -> 
	      if k = k' then (k,v)::rest
	      else ins rest
	  | [] -> [(k,v)]
      in
      m := ins (!m)
    let fold f m b = List.fold_left (fun b (k,v) -> f (k,v) b) b (!m)  
  end
 
module HashMap : MAP =
  struct
    type ('a,'b) t = ('a, 'b) Hashtbl.t
    let empty () = Hashtbl.create 97
    let get m k = Hashtbl.find m k
    let insert m k v = Hashtbl.replace m k v
    let fold f m b = Hashtbl.fold (fun k v c -> f (k,v) c) m b  
  end

type ('a,'b) map =
    List of ('a,'b) ListMap.t  
  | Hash of ('a,'b) HashMap.t 

(* END MAP DEFINITIONS *)

type node = int

type src = node
type dst = node
type weight = float

(* type of decompositions *)
type 'a map_t = HashT of 'a | ListT of 'a
type dcomp_t =
    BaseT 
  | SrcT of dcomp_t map_t
  | DstT of dcomp_t map_t
  | PairT of dcomp_t map_t * dcomp_t map_t

(* defines the good decomposition types:
 * ensures Src & Dst appears at most once on each path to a leaf. *) 
type used = Used | Unused
let good_dcomp_t (dt:dcomp_t) : bool = 
  let rec aux dt src dst =
    match dt, src, dst with
	BaseT, _, _ -> true
      | SrcT mt, Unused, _ -> aux_map mt Used dst
      | DstT mt, _, Unused -> aux_map mt src Used
      | PairT (mt1,mt2), _, _ -> aux_map mt1 src dst && aux_map mt2 src dst
      | _, _, _ -> false
  and aux_map mt src dst =
    match mt with
	HashT dt -> aux dt src dst
      | ListT dt -> aux dt src dst
  in
  aux dt Unused Unused

(* decomposition instances *)
type dcomp =
    BaseD of weight 
  | SrcD of (src, dcomp) map
  | DstD of (dst, dcomp) map
  | PairD of dcomp * dcomp

(* user queries *)
type query =
    AllQ
  | SrcQ of src
  | DstQ of dst
  | SrcDstQ of src * dst

(* paths that implement queries in decompositions *)
type path = 
    EndP 
  | SrcP of src * path 
  | DstP of dst * path 
  | LeftP of path  
  | RightP of path

(* good_pq p q is true if p can implement the query q *)
let rec good_pq p q =
  match p,q with
    | EndP, AllQ                  -> true
    | SrcP (s,p), SrcQ s'         -> s = s' && good_pq p AllQ
    | SrcP (s,p), SrcDstQ (s',d') -> s = s' && good_pq p (DstQ d')
    | DstP (d,p), DstQ d'         -> d = d' && good_pq p AllQ
    | DstP (d,p), SrcDstQ (s',d') -> d = d' && good_pq p (SrcQ s')
    | LeftP p, q                  -> good_pq p q
    | RightP p, q                 -> good_pq p q
    | _, _                        -> false    
   
(* good_pd p dt is true if path p navigates through values of type dcomp_t *)
let rec good_pd (p:path) (dt:dcomp_t) : bool =
  match p, dt with
      EndP, BaseT -> true
    | SrcP (s,p), SrcT dm -> good_pm p dm
    | DstP (d,p), DstT dm  -> good_pm p dm
    | LeftP p, PairT (dm1, dm2) -> good_pm p dm1
    | RightP p, PairT (dm1, dm2) -> good_pm p dm2
    | _, _ -> false
and good_pm (p:path) (dm:dcomp_t map_t) : bool =
  match dm with
      HashT dt -> good_pd p dt
    | ListT dt -> good_pd p dt

(*

let get (d:dcomp) (p:path) =
  match p, d with
      EndP, _ -> d
    | SrcO
*)

(*

* : t    -- allocate a symbolic value of type t

* : t where p -- allocate a symbolic value v of type t where p v satisfied
              -- could be abbreviation for (let x = * : t in assert p x; x)

Example client:

let t = * : dcomp_t where good_dcomp_t in  (* create new dcomp_t *)
let e = empty t in                         (* create empty map of type t *)
insert t e k1 v1;                          (* add some values *)
insert t e k2 v2;
query t (SrcDst (n1,n2)) e in

Here's a query function:

let query (t:dcomp_t) (q:query) (d:dcomp) : dcomp =
  let p = * : path where (fun p -> good_pq p q && good_pd p t) in
  query t p d

=========

The good thing about the above is that the query function hides the complexity
of what is a "good" path and how to allocate the path from the user.
The bad is that I was thinking that each syntactic occurrence of * gives you 1 symbolic
value (ie:  you get 1, not 1 for each time the code is executed)

An alternate possibility is you allocate the * in the client.  But then you
need a general "assert" function.  And you also have to reveal to the client that
you are doing some kind of symbolic search, which is not so good.  See here:

query t (SrcDst (n1,n2)) e ( * ) in

Here's a query function:

let query (t:dcomp_t) (q:query) (d:dcomp) (p: path) =
  assert (fun p -> good_pq p q && good_pd p t) p in
  query t p d

Perhaps the query function just needs to be a macro so it is unfolded many times --
once for each use in the source file at compile time.

*)
