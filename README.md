# point
Presentation Of Interactive Normalform Transformations

Needs "js_of_ocaml" to build the web interface, as it does "dune" to easily build everything. 
Both can be installed using opam: `opam install js_of_ocaml js_of_ocaml-ppx js_of_ocaml-tyxml dune`. Opam is available in most Linux distros through the package manager, and there is a port for Windows and MacOS, [on their webpage](https://opam.ocaml.org/).


## How to Build


#### To build the web interface: 
`dune build point/normform_web.js --profile release` 

This produces the needed .js file in the _build directory, this must be moved to the same location as the "point.html" file. Or you adapt the `<script>` tag in the html file, whatever seems easier. 

#### To bulid the command line interface:
`dune build point/normform_cli.exe --profile release`

This produces a .exe file (yes, it always uses the .exe ending for execuables) in the _build directory. Can be run directly in the shell `./normform_cli.exe`