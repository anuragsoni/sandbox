val fork : (unit -> unit) -> unit
val wait_read : Unix.file_descr -> unit
val wait_write : Unix.file_descr -> unit
val read : Unix.file_descr -> bytes -> pos:int -> len:int -> [ `Ok of int | `Eof ]
val write : Unix.file_descr -> bytes -> pos:int -> len:int -> [ `Ok of int | `Eof ]
val accept : ?cloexec:bool -> Unix.file_descr -> Unix.file_descr * Unix.sockaddr
val run : (unit -> unit) -> unit
