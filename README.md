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

# TODO
- the intention was to let people manually annotate required external modules with `[@package]` annotations to ensure they get put in the `packages` binding in `config.ml`. I cannot get this to work, so for now we try to infer it from the set of installed packages using findlib, which I found works relatively well.

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