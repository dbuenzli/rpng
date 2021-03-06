Rpng — Simple PNG codec
-------------------------------------------------------------------------------
Release %%VERSION%%

Home page: http://erratique.ch/software/rpng    
Contact: Daniel Bünzli `<daniel.buenzl i@erratique.ch>`


## Installation

Rpng can be installed with `opam`:

    opam install rpng

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.


## Documentation

The documentation and API reference is automatically generated by
`ocamldoc` from the interfaces. It can be consulted [online][3]
and there is a generated version in the `doc` directory of the 
distribution. 

[3]: http://erratique.ch/software/rpng/doc/Rpng


## Sample programs

If you installed Rpng with `opam` sample programs are located in
the directory `opam config var rpng:doc`.

In the distribution sample programs are located in the `test`
directory of the distribution. They can be built with:

    ocamlbuild -use-ocamlfind test/tests.otarget

The resulting binaries are in `_build/test`.
