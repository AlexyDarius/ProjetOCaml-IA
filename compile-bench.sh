#!/bin/bash
ocamlopt -c dict.mli
echo "dict.mli compilé"
ocamlopt -c constraints.mli
echo "constraints.mli compilé"
ocamlopt -c readfile.mli
echo "readfile.mli compilé"
ocamlopt -c interface.mli
echo "interface.mli compilé"
ocamlopt -c backtrack.mli
echo "backtrack.mli compilé"
ocamlopt -o bench str.cmxa dict.ml constraints.ml readfile.ml interface.ml backtrack.ml benchmark.ml
if (($?==0))
then
    echo "Programme main compilé, lancez ./bench"
else
    echo "Erreur à la compilation"
fi