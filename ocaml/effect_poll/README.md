# Effect_poll

A very simple event loop written using algebraic effects support in OCaml 5.

**Features**:
- Direct-style async io using effect.
- Use the host operating system's native I/O multiplexing syscall. Rely on [poll](https://github.com/anuragsoni/poll) as an abstraction for epoll (on linux), kqueue (on macOS) and wepoll (IOCP on windows).

**Todo**:
- Add support for timers
- Add support for yielding threads