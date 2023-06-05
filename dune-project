(lang dune 3.8)

(name dnstoy)

(generate_opam_files true)

(source
 (uri https://git.mylloon.fr/Anri/dns-toy.git))

(authors "Mylloon")

(maintainers "Mylloon")

(license AGPL-3.0-or-later)

(documentation https://implement-dns.wizardzines.com/book/intro.html)

(package
 (name dnstoy)
 (synopsis "DNS implementation (toy)")
 (description "Implement DNS in a weekend, in OCaml")
 (depends ocaml dune)
 (tags (dns)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
