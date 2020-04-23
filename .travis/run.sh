#!/usr/bin/env bash

#
# Because the ocaml/opam2 docker image uses a stale
# local copy of opam repo, we just delete everything
# and start from scratch :)
#
rm -rf ~/.opam

opam init --compiler="${OCAML_VERSION}"
opam update

eval `opam env`
opam config report

if [ -z "${MIN_REQS_ONLY}" ]; then
    opam install --yes --deps-only --with-test ./ExcelSynth.opam
else
    opam install --yes alcotest.0.8.0   \
                       core.v0.13.0     \
                       csv.2.3          \
                       dune.1.11.0      \
                       lwt.4.2.0
fi

opam list
pwd ; ls -lah

dune clean

dune build --verbose
dune build bin/App.exe --verbose

dune runtest --verbose