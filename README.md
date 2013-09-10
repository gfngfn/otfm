Otfm — OpenType font decoder for OCaml
-------------------------------------------------------------------------------
Release %%VERSION%%

Otfm is an in-memory decoder for the OpenType font data format. It
provides low-level access to tables of OpenType fonts and functions to
decode some of them.
   
Otfm is made of a single, independent module and is distributed under
a BSD3 license.
     
Home page: http://erratique.ch/software/otfm  
Contact: Daniel Bünzli `<daniel.buenzl i@erratique.ch>

## Installation

Otfm can be install with `opam`:

    opam install opam

If you don't use `opam` consult the [`opam`](opam) file for build
instructions and a complete specification of the dependencies. 

## Documentation 

The documentation and API reference is automatically generated 
by `ocamldoc` from `otfm.mli`. It can be consulted [online][1] and
there is a generated version in the `doc` directory of the
distribution. 

[1]: http://erratique.ch/software/otfm/doc/Otfm

## Sample programs 

Sample programs are located in the `test` directory of the
distribution. They can be built with:

    ocamlbuild test/tests.otarget 
    
The resulting binaries are in `_build/test`:

- `test.byte` tests the library, nothing should fail.
- `otfpp.native`, pretty prints OpenType files invoke with `--help`
  for more information.
