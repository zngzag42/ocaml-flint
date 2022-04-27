(lang dune 3.0)
(using ctypes 0.1)
(using dune_site 0.1)
(name oflint)

(generate_opam_files true)

(authors "Zachary Greenberg")
(maintainers "zgreenberg@math.uni-heidelberg.de")

(package
 (name oflint)
 (synopsis "Ocaml binding for Flint (Fast LIbray for Number Theory)")
 (description "Supports  arbitrary prescision polynomials in one or multiple variables")
 )
