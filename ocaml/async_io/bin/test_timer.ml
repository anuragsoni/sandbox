open Async_io

let () =
  let promise = Promise.create () in
  Scheduler.run (fun () ->
      let now = Ptime_clock.now () in
      Format.printf "Now (%d): %a\n%!" (Domain.self () :> int) Ptime.pp now;
      Timer.sleep Mtime.Span.s;
      let now = Ptime_clock.now () in
      Format.printf "Now (%d): %a\n%!" (Domain.self () :> int) Ptime.pp now;
      Promise.resolve promise ());
  Promise.await promise
;;