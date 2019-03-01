SHELL=/bin/bash

# Cross-platform realpath from 
# https://stackoverflow.com/a/18443300
# NOTE: Adapted for Makefile use
define BASH_FUNC_realpath%%
() {
  OURPWD=$PWD
  cd "$(dirname "$1")"
  LINK=$(readlink "$(basename "$1")")
  while [ "$LINK" ]; do
    cd "$(dirname "$LINK")"
    LINK=$(readlink "$(basename "$1")")
  done
  REALPATH="$PWD/$(basename "$1")"
  cd "$OURPWD"
  echo "$REALPATH"
}
endef
export BASH_FUNC_realpath%%

ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

.PHONY: help
.PHONY: lsee
.PHONY: redis
.PHONY: nginx
.PHONY: hexchat
.PHONY: nmap
.PHONY: curl
.PHONY: rq4
.PHONY: linux
.PHONY: collect

.DEFAULT_GOAL := help

help: ## This help.
	@grep -E \
		'^[\/\.0-9a-zA-Z_-]+:.*?## .*$$' \
		$(MAKEFILE_LIST) \
		| sort \
		| awk 'BEGIN {FS = ":.*?## "}; \
		       {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

lsee: ## Ensures that the lsee image is pulled from docker hub. 
	@echo "[lsee] Ensuring we have lsee"
	docker pull jjhenkel/lsee

lsee-no-df: ## Ensures we have the lsee:no-df image.
	@echo "[lsee] Ensuring we have lsee:no-df"
	docker pull jjhenkel/lsee:no-df

redis: lsee ## Generates traces for redis and creates merged trace corpus.
	@echo "[lsee] Generating traces for redis..."
	@echo "[lsee] Using $$(getconf _NPROCESSORS_ONLN) workers..."
	${ROOT_DIR}/lsee-all ../c2ocaml/artifacts/redis
	time make -f ./artifacts/Makefile.temp -j$$(getconf _NPROCESSORS_ONLN) all
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	@echo "[lsee] Generated traces! Run 'NAME=redis make collect' to build a trace corpus."

nginx-no-df: lsee-no-df ## Generates traces for nginx and creates merged trace corpus.
	@echo "[lsee] Generating traces for nginx..."
	@echo "[lsee] Using $$(getconf _NPROCESSORS_ONLN) workers..."
	${ROOT_DIR}/lsee-all-no-df ../c2ocaml/artifacts/nginx
	time make -f ./artifacts/Makefile.temp -j$$(getconf _NPROCESSORS_ONLN) all
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	@echo "[lsee] Generated traces! Run 'NAME=nginx-no-df make collect' to build a trace corpus."

hexchat-no-df: lsee-no-df ## Generates traces for hexchat and creates merged trace corpus.
	@echo "[lsee] Generating traces for hexchat..."
	@echo "[lsee] Using $$(getconf _NPROCESSORS_ONLN) workers..."
	${ROOT_DIR}/lsee-all-no-df ../c2ocaml/artifacts/hexchat
	time make -f ./artifacts/Makefile.temp -j$$(getconf _NPROCESSORS_ONLN) all
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	@echo "[lsee] Generated traces! Run 'NAME=hexchat-no-df make collect' to build a trace corpus."

nmap-no-df: lsee-no-df ## Generates traces for nmap and creates merged trace corpus.
	@echo "[lsee] Generating traces for nmap..."
	@echo "[lsee] Using $$(getconf _NPROCESSORS_ONLN) workers..."
	${ROOT_DIR}/lsee-all-no-df ../c2ocaml/artifacts/nmap
	time make -f ./artifacts/Makefile.temp -j$$(getconf _NPROCESSORS_ONLN) all
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	@echo "[lsee] Generated traces! Run 'NAME=nmap-no-df make collect' to build a trace corpus."

curl-no-df: lsee-no-df ## Generates traces for curl and creates merged trace corpus.
	@echo "[lsee] Generating traces for curl..."
	@echo "[lsee] Using $$(getconf _NPROCESSORS_ONLN) workers..."
	${ROOT_DIR}/lsee-all-no-df ../c2ocaml/artifacts/curl
	time make -f ./artifacts/Makefile.temp -j$$(getconf _NPROCESSORS_ONLN) all
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	@echo "[lsee] Generated traces! Run 'NAME=curl-no-df make collect' to build a trace corpus."

rq4: lsee-no-df ## Generates traces for rq4 and creates good/bad trace corpora.
	@echo "[lsee] Generating traces for rq4..."
	@echo "[lsee] Using $$(getconf _NPROCESSORS_ONLN) workers..."
	${ROOT_DIR}/lsee-all ../c2ocaml/artifacts/rq4 no-df
	time make -f ./artifacts/Makefile.temp -j$$(getconf _NPROCESSORS_ONLN) all
	@echo "[lsee] Creating trace corpus ${ROOT_DIR}/rq4-good.traces.txt"
	cat ${ROOT_DIR}/artifacts/rq4-*-good.c.traces > "${ROOT_DIR}/rq4-good.traces.txt"
	@echo "[lsee] Corpus created with $$(cat ${ROOT_DIR}/rq4-good.traces.txt | wc -l) traces."
	@echo "[lsee] Creating trace corpus ${ROOT_DIR}/rq4-bad.traces.txt"
	cat ${ROOT_DIR}/artifacts/rq4-*-bad.c.traces > "${ROOT_DIR}/rq4-bad.traces.txt"
	@echo "[lsee] Corpus created with $$(cat ${ROOT_DIR}/rq4-bad.traces.txt | wc -l) traces."
	@rm -f ${ROOT_DIR}/artifacts/*.traces
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	@echo "[lsee] Cleaned up ${ROOT_DIR}/artifacts"
	@echo "[lsee] All done."

linux: lsee ## Generates traces for linux v4.5-rc4 and creates merged trace corpus.
	@echo "[lsee] Generating traces for linux (WARNING: VERY SLOW)..."
	@echo "[lsee] Using $$(getconf _NPROCESSORS_ONLN) workers..."
	${ROOT_DIR}/lsee-all ../c2ocaml/artifacts/linux
	time make -f ./artifacts/Makefile.temp -j$$(getconf _NPROCESSORS_ONLN) all
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	@echo "[lsee] Generated traces! Run 'NAME=linux make collect' to build a trace corpus."

linux-no-df: lsee-no-df ## Generates traces for linux v4.5-rc4 using no-df configuration.
	@echo "[lsee] Generating traces for linux (WARNING: VERY SLOW)..."
	@echo "[lsee] Using $$(getconf _NPROCESSORS_ONLN) workers..."
	${ROOT_DIR}/lsee-all-no-df ../c2ocaml/artifacts/linux
	time make -f ./artifacts/Makefile.temp -j$$(getconf _NPROCESSORS_ONLN) all
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	@echo "[lsee] Generated traces! Run 'NAME=linux-no-df make collect' to build a trace corpus."

collect: ## REQUIRES PARAMETER 'NAME=<name>' : collects all traces in the artifacts directory into a trace corpus.
	@echo "[lsee] Creating trace corpus ${ROOT_DIR}/${NAME}.traces.gz"
	find ./artifacts/ -type f -name "*.traces" -print0 | xargs -0 -n1 -I'{}' bash -c "cat {} | gzip - >> ${ROOT_DIR}/${NAME}.traces.gz"
	@echo "[lsee] Corpus created with $$(cat ${ROOT_DIR}/${NAME}.traces.gz | gzip -cd | wc -l) traces."
	@find ./artifacts/ -type f | xargs rm -f
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	@echo "[lsee] Cleaned up ${ROOT_DIR}/artifacts"
	@echo "[lsee] All done. Use GloVe to learn embeddings from your trace corpus!"
