Testing saturn MS queue.

## Build and run

```
opam switch create 5.1.1
eval $(opam env)
opam install dune batteries progress ptime cmdliner saturn domainslib
dune build
dune exec saturn_test
```