module SimpleTreiber = struct
  type 'a t = 'a list Atomic.t

  let create () = Atomic.make []

  let rec push t v =
    let cur = Atomic.get t in
    if not (Atomic.compare_and_set t cur (v :: cur)) then
      push t v

  let pop_all t = Array.of_list (Atomic.exchange t [])
end