# Adapted from https://github.com/akabe/docker-ocaml/blob/master/dockerfiles/ubuntu16.04_ocaml4.06.1/Dockerfile

FROM ubuntu:18.04

LABEL maintainer="padhi@cs.ucla.edu"


ENV OPAM_VERSION  2.0.6
ENV OCAML_VERSION 4.09.0+flambda

ENV HOME /home/opam


ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && \
    apt-get upgrade -yq && \
    apt-get install -yq aspcud \
                        binutils \
                        cmake curl \
                        g++ git \
                        libgmp-dev libgomp1 libomp5 libomp-dev libx11-dev \
                        m4 make \
                        patch python3 python3-distutils \
                        sudo \
                        time tzdata \
                        unzip \
                        && \
    apt-get autoremove -y --purge


RUN adduser --disabled-password --home $HOME --shell /bin/bash --gecos '' opam && \
    echo 'opam ALL=(ALL) NOPASSWD:ALL' >>/etc/sudoers && \
    curl -L -o /usr/bin/opam "https://github.com/ocaml/opam/releases/download/$OPAM_VERSION/opam-$OPAM_VERSION-$(uname -m)-$(uname -s)" && \
    chmod 755 /usr/bin/opam && \
    su opam -c "opam init --auto-setup --disable-sandboxing --yes --compiler=$OCAML_VERSION && opam clean"


USER opam
WORKDIR $HOME


RUN opam install --yes alcotest.0.8.5 \
                       core.v0.13.0 \
                       csv.2.4 \
                       dune.2.3.0 \
                       ppx_let.v0.13.0 \
                       && \
    opam clean --yes && \
    git clone https://github.com/SaswatPadhi/ExcelSynth.git


WORKDIR $HOME/ExcelSynth


ENV LC_CTYPE=C.UTF-8
RUN opam config exec -- dune build && dune runtest


ENTRYPOINT [ "opam", "config", "exec", "--" ]
CMD [ "bash" ]