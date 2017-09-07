let error s = failwith s

(********************************)
(* Schema for Rep Synth Example *)
(********************************)

type state = R | S
type col = NS of int | PID of int | State of state | CPU of int

type tuple = col list  (* should have distinct columns *)
let eq_tuple t1 t2 = (t1 = t2)

(************************)
(* Pretend Partial Maps *)
(************************)

type ('a,'b) map = ('a * 'b) ref

exception NotFound

let lookup eq m key =
  let (k,v) = !m in
  if eq key k then
    v
  else
    raise NotFound

let insert eq m key value =
  m := (key,value)

(**************************)
(* Representation choices *)
(**************************)

type dag =
    Base of tuple             (* bottom of dag *)
  | One of link               (* current node has one child *)
  | Two of link * link        (* current node has two children *)
and link = (tuple, dag) map   (* in future, could have several kinds of maps
				 such as hashtable, vectors, etc *)

(* path through a dag *)
type path =
    End                        (* arrive at tuple stored in dag *)
  | Next of tuple * path       (* take One descendent in dag *)
  | Left of tuple * path       (* take Left child of Two *)
  | Right of tuple * path      (* take Right child of Two *)

let rec read_dag (p:path) (d:dag) : tuple =
  match p, d with
      End, Base t -> t
    | Next (t1,p1), One link -> read_dag p1 (lookup eq_tuple link t1)
    | Left (t1,p1), Two (l,r) -> read_dag p1 (lookup eq_tuple l t1)
    | Right (t1,p1), Two (l,r) -> read_dag p1 (lookup eq_tuple r t1)
    | _, _ -> error "bad path"

(*****************************************************)
(* Representation choices guided by symbolic choices *)
(*****************************************************)

(*
type choices = 
  Base 
| One of ctree 
| Two of ctree * ctree
and ctree = choices symbol

type rep (cs:ctree) =
  case cs of
  | Base -> tuple
  | One cs1 -> (tuple, rep cs1) map
  | Two (csl,csr) -> (tuple, rep csl) map * (tuple, rep csr) map

let rec read_dag (cs:ctree) (p:path) (d:rep cs) : tuple =
  match p, cs with
    | End, Base -> d
    | Next (t1,p1), One cs1  -> read_dag cs1 p1 (lookup eq_tuple d t1)
    | Left (t1,p1), Two (csl,csr) -> read_dag p1 (lookup eq_tuple (fst d) t1)
    | Right (t1,p1), Two (csl,csr) -> read_dag p1 (lookup eq_tuple (snd d) t2)
    | _, _ -> error "bad path"
*)

(**************)
(* OLDER WORK *)
(**************)

(* Representation of the example (substituting refs for maps) 
   & read function in rep synth paper *)

type wT = col list ref
type ywT = (col list * wT) ref
type zwT = (col list * wT) ref
type xT = (col list * ywT * zwT) ref
    
let wRead (key:col list) (data:wT) : col list =
  match key with
      [] -> !data
    | _ -> error "w read bad key"

let yRead (key:col list) (data:ywT) : col list =
  let (ydata,wdata) = !data in
  match key, ydata with
      (PID n)::_, (PID m)::_ -> 
	if m = n then wRead [] wdata 
	else []
    | _, _ -> error "y read bad key or data"

let zRead (key:col list) (data:zwT) : col list =
  let (zdata,wdata) = !data in
  match key, zdata with
      (NS n)::(PID p)::_, (NS m)::(PID q)::_ -> 
	if m = n && p = q then wRead [] wdata 
	else []
    | _, _ -> error "z read bad key or data"

type path = Left of col list * col list | Right of col list * col list

let xRead (path:path) (data:xT) =
  let (xdata,ydata,zdata) = !data in
  match path with
      Left (xkey,ykey) ->
	(match xkey, xdata with
	    (NS n)::_, (NS m)::_ -> 
	      if n = m then yread ykey ydata
	      else []
	  |_, _ ->  error "x read bad key or data on left")
    | Right (xkey,zkey) ->
	(match xkey, xdata with
	    (NS _)::(State r)::_, (State s)::_ -> 
	      if r = s then zread zkey zdata
	      else []
	  |_, _ ->  error "x read bad key or data on right")


(* Main *)

let p s = print_endline s 

let main () =
  p "hello world"

let _ = main ()
