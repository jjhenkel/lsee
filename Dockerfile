FROM ocaml/opam:debian-9_ocaml-4.05.0_flambda

RUN sudo apt-get install -y \
  libgmp-dev \
  libmpfr-dev \
  make \
  automake \
  autoconf \
  python3 \
  python3-dev

RUN sudo -u opam sh -c "opam update && opam install --no-checksums -y \
  jbuilder\
  easy-format\
  stdio\
  re\
  batteries\
  hashcons\
  zarith\
  mlgmpidl\
"

RUN sudo git clone https://github.com/Z3Prover/z3.git /z3 && \
 cd /z3 && sudo git checkout tags/z3-4.6.0 && \
 sudo chown -R opam /z3

RUN opam init -y && eval `opam config env` && \
  cd /z3 && python3 scripts/mk_make.py -g --ml && \ 
  cd build && make -j$(getconf _NPROCESSORS_ONLN) && sudo "PATH=$PATH" make install && \
  ocamlfind install z3 /home/opam/.opam/4.05.0+flambda/lib/Z3/*

COPY . /app 
WORKDIR /app

RUN sudo rm -f /etc/ocamlfind.conf && sudo mv /app/opt/ocamlfind.conf /etc && \
  sudo chown -R opam /app && \
  ulimit -s unlimited && \
  opam init -y && eval `opam config env` && \
  jbuilder build driver/main.exe

ENTRYPOINT ["./entrypoint.sh"]
