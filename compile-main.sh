#!/bin/bash
ocamlopt -c dict.mli
echo "dict.mli compilé"
ocamlopt -c readfile.mli
echo "readfile.mli compilé"
ocamlopt -c constraints.mli
echo "constraints.mli compilé"
ocamlopt -c interface.mli
echo "interface.mli compilé"
ocamlopt -c backtrack.mli
echo "backtrack.mli compilé"
ocamlopt -o main str.cmxa dict.ml constraints.ml readfile.ml interface.ml backtrack.ml main.ml
if (($?==0))
then
    echo "Programme main compilé, lancez ./main"
else
    echo "Erreur à la compilation"
fi
