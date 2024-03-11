(lang dune 3.0)

(name mappl)

(using menhir 2.1)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name mappl)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
  (ocaml (= 4.14))
  (dune (= 3.12.2))
  (dune-build-info (= 3.12.2))
  (core (= 0.16.2))
  (menhir (= 20231231))
  (menhirLib (= 20231231))
  (ppx_jane (= 0.16.0)) 
  (ppx_deriving (= 5.2.1)) 
  (core_unix (= 0.16.0))
  (core_unix.filename_unix (= 0.16.0))
  (core_unix.command_unix (= 0.16.0))
  (core_unix.sys_unix (= 0.16.0))
  str
  )
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
