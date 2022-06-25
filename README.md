# Anurag's sandbox

Small snippets of code in various programming languages, used for exploring different programming languages, learning about new programming language constructs or just to have an easy-to-use example for later reference.

- [Effect_poll](./ocaml/effect_poll/)
    - Learning how to use OCaml 5's algebraic effects by writing a simple event loop
    - Direct-style nonblocking IO using effects + [poll](https://github.com/anuragsoni/poll). No monads in sight!!
- [Executor](./ocaml/executor/)
    - Experiments with a simple multi-threaded task executor
- [Async_io](./ocaml/async_io/)
    - Learning how a work-stealing multi-threade task executor works.
    - Suitable for asynchronous I/O. Direct-style cooperative tasks usinf effects, and uses [poll](https://github.com/anuragsoni/poll) for leveraging the operating system's readiness event notifications for file descriptors.
    - Timers for managing timed ocaml effects.