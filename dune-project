(lang dune 3.20)
(using menhir 3.0)

(name aps)
(license "GPL-3.0-or-later")

(generate_opam_files false)

(authors "Loic Sylvestre" "Danaël Carbonneau" "Pascal Manoury")

(maintainers "Danaël Carbonneau <danael.carbonneau@lip6.fr>")


(package
 (name aps)
 (depends ocaml menhir bos)
)

