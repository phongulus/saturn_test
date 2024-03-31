module T = Domainslib.Task
open Saturn_test.Timing

(* Select queue for testing*)
module Queue = Saturn.Queue
(* module Queue = Saturn_test.Treiber.SimpleTreiber *)

let domains = Array.init 24 (fun i -> i + 1)
let q_init = 200_000
let push_ops = 2_000_000
let pop_ops = 2_000_000

type array_op =
  | Push of int
  | Pop

(* https://discuss.ocaml.org/t/more-natural-preferred-way-to-shuffle-an-array/217 *)
let knuth_shuffle a =
  let n = Array.length a in
  let a = Array.copy a in
  for i = n - 1 downto 1 do
    let k = Random.int (i+1) in
    let x = a.(k) in
    a.(k) <- a.(i);
    a.(i) <- x
  done;
  a

let init () =
  let init_arr = Array.init q_init (fun _ -> Random.full_int max_int) in
  let q = Queue.create () in
  Array.iter (fun n -> Queue.push q n) init_arr;
  let arr = Array.init (push_ops + pop_ops) (fun i ->
    if i < push_ops
    then Push (Random.full_int max_int)
    else Pop) in
  (knuth_shuffle arr, q)

let bench pool (arr, q) =
  T.parallel_for pool ~chunk_size:1 ~start:0 ~finish:(Array.length arr - 1)
    ~body:(fun i ->
      match arr.(i) with
      | Push k -> Queue.push q k
      | Pop -> ignore @@ Queue.pop q)

let () =
  Array.iter (fun num_domains ->
    Printf.printf "%d domains: " num_domains;
    let pool = T.setup_pool ~num_domains () in
    T.run pool (fun () -> time ~no_warmup:5 ~no_iter:5 ~init (bench pool));
    T.teardown_pool pool
  ) domains
