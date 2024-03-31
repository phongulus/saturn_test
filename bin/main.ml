module T = Domainslib.Task
open Saturn
open Saturn_test.Timing

let domains = Array.init 16 (fun i -> i + 1)
let q_init = 2_000_000
let arr_ops = 5_000_000

let init () =
  let init_arr = Array.init q_init (fun _ -> Random.full_int max_int) in
  let arr = Array.init arr_ops (fun _ -> Random.full_int max_int) in
  let q = Queue.create () in
  Array.iter (fun n -> Queue.push q n) init_arr;
  (arr, q)

let bench pool (arr, q) =
  T.parallel_for pool ~chunk_size:1 ~start:0 ~finish:(Array.length arr - 1)
    ~body:(fun i -> Queue.push q arr.(i));
  T.parallel_for pool ~chunk_size:1 ~start:0 ~finish:(Array.length arr - 1)
    ~body:(fun _ -> ignore @@ Queue.pop q)

let () =
  Array.iter (fun num_domains ->
    let pool = T.setup_pool ~num_domains () in
    T.run pool (fun () -> time ~no_warmup:5 ~no_iter:5 ~init (bench pool));
    T.teardown_pool pool
  ) domains
