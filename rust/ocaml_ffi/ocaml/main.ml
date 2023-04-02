external hello_from_rust : unit -> unit = "ffi_example_rust_hello"

let () = hello_from_rust ()
