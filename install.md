# Installation Guide

## Install External libraries

```shell
opam install core
opam install cohttp-lwt-unix cohttp-async
opam install tls
opam install ssl
opam install lwt.4.5.0
```

## Start Application

Run the following command in the terminal

```shell
make build
make read
```

If `make read` does not work, run the following command:

```ocaml
make
#use "main.ml"
```
