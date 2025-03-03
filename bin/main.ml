let map pool f l =
    Eio.Fiber.List.map
      (fun elem ->
        Eio.Executor_pool.submit_exn pool ~weight:0.5 (fun () -> f elem))
      l

(* global state *)
let c = ref 0
let incr x = c := x + !c

(* tab n ~= [n;n-1;...;0]*)
let rec tab x = 
  if x = 0 
  then [ x ]
  else x :: tab (x - 1)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pool =
    Eio.Executor_pool.create
      ~sw (Eio.Stdenv.domain_mgr env)
      ~domain_count:11
  in
  map pool incr (tab 10000) |> ignore
