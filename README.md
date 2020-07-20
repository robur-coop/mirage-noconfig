# mirage-noconfig

This tool aims to help MirageOS unikernel authors synthesize the contents of the
`config.ml` file by analyzing the unikernel implementation.

# Usage

```shell
mirage-noconfig $ opam pin add '#master'
my-project $ ls
  unikernel.ml
my-project $ noconfig > config.ml
my-project $ mirage configure -t unix
my-project $ make depend
my-project $ make
my-project $ ./main.native

```

# Registering external dependencies

Usually the external packages/libraries required are signalled to the `mirage configure` tool with the `foreign ~packages` argument.

Since `noconfig` generates a new `config.ml`, a different mechanism is required.

`noconfig` will look for `package` annotations in the top level in your unikernel file, like this:

```ocaml
[@@@package "ipaddr"]
[@@@package "tcpip" {| >= "0.2" |}  ]
[@@@package "foo"   {| >= "1.0" & < "2.0" |}]

module Main = struct
  (* No-op unikernel *)
  let start () = Lwt.return ()
end
```

This will result in packages `ipaddr`, `tcpip`, and `foo` being registered, and the bounds provided as the optional second parameter are passed on to `opam`.

# TODO / pain points
- mirage configure doesn't work when run in a local git repo without a 'origin' remote
- bootvars / key_gen doesn't work well in tooling
  - could pass in arguments to the job as a functor
```ocaml
module Main (K : Arguments) = struct
  let abc = K.get (opt int 123) ["hello";"hi"]
end
module type Arguments : sig
  val get : 'a.'a argument -> flag list -> 'a
end
  - would be nice if unikernel can only access its own bootvars
   - would be nice if clashing args fail on configure
