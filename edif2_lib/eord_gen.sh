#!/bin/sh
# Regenerates eord.ml (EDIF keyword <-> token maps) from edif2.mli.
# NOTE: uses lowercase_first (only first char) for the symbol table key;
# the lexer must match this convention. The bare single-letter EDIF
# keyword "E" (scaled-number (e mant exp)) collides with Vivado signal
# names like E[7:0] -- exclude it (see task #16 / SVS bug #2).
echo open String > eord.ml
echo let esymbols = Hashtbl.create 256 >> eord.ml
echo 'let lowercase_first str = String.make 1 (Char.lowercase_ascii str.[0]) ^ String.sub str 1 (String.length str - 1)' >> eord.ml
echo 'let _ = List.iter (fun (str,key) -> if str <> "" then Hashtbl.add esymbols (lowercase_first str) key) [' >> eord.ml
grep \| edif2.mli | tr '\011|' ' ' | sed -e 's=[\ ]*\([A-Z][A-Za-z0-9_]*\)=("\1", Edif2.\1=' -e 's= of (unit)= ()=' -e 's=$=);=' | grep -v ' of ' | grep -v '("E", Edif2.E)' >> eord.ml
echo '("", Edif2.EMPTYEDIF)]' >> eord.ml
echo let getstr tok = match tok with >> eord.ml
grep \| edif2.mli | cut -d\( -f1 | tr '\011' ' ' |\
sed -e 's=|[\ ]*\([A-Z][A-Za-z0-9_\ of]*\)=|\ Edif2.\1 -> (\"\1\")=' -e 's= of= arg=' -e 's= of ==' >> eord.ml
