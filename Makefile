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
.PHONY: redis
.PHONY: nginx
.PHONY: hexchat
.PHONY: nmap
.PHONY: curl
.PHONY: linux

.DEFAULT_GOAL := help

help: ## This help.
	@grep -E \
		'^[\/\.0-9a-zA-Z_-]+:.*?## .*$$' \
		$(MAKEFILE_LIST) \
		| sort \
		| awk 'BEGIN {FS = ":.*?## "}; \
		       {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

redis: ## Generates traces for redis and creates merged trace corpus.
	@echo "[lsee] Generating traces for redis (SAMPLE=NONE)..."
	${ROOT_DIR}/lsee-all ../c2ocaml/artifacts/redis ml4spec
	time make -f ./artifacts/Makefile.temp -j$$(getconf _NPROCESSORS_ONLN) all
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	@find ./artifacts/ -type f -name "redis-*.traces" -print0 | xargs -0 -n1 -I'{}' bash -c "cat {} | gzip - >> ${ROOT_DIR}/redis-raw.traces.gz"
	@find ./artifacts/ -type f -name "redis-*" | xargs rm -f
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	${ROOT_DIR}/do-samples.sh redis
	@rm -f ${ROOT_DIR}/redis-raw.traces.gz

nginx: ## Generates traces for nginx and creates merged trace corpus.
	@echo "[lsee] Generating traces for nginx (SAMPLE=NONE)..."
	${ROOT_DIR}/lsee-all ../c2ocaml/artifacts/nginx ml4spec
	time make -f ./artifacts/Makefile.temp -j$$(getconf _NPROCESSORS_ONLN) all
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	@find ./artifacts/ -type f -name "nginx-*.traces" -print0 | xargs -0 -n1 -I'{}' bash -c "cat {} | gzip - >> ${ROOT_DIR}/nginx-raw.traces.gz"
	@find ./artifacts/ -type f -name "nginx-*" | xargs rm -f
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	${ROOT_DIR}/do-samples.sh nginx
	@rm -f ${ROOT_DIR}/nginx-raw.traces.gz

hexchat: ## Generates traces for hexchat and creates merged trace corpus.
	@echo "[lsee] Generating traces for hexchat (SAMPLE=NONE)..."
	${ROOT_DIR}/lsee-all ../c2ocaml/artifacts/hexchat ml4spec
	time make -f ./artifacts/Makefile.temp -j$$(getconf _NPROCESSORS_ONLN) all
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	@find ./artifacts/ -type f -name "hexchat-*.traces" -print0 | xargs -0 -n1 -I'{}' bash -c "cat {} | gzip - >> ${ROOT_DIR}/hexchat-raw.traces.gz"
	@find ./artifacts/ -type f -name "hexchat-*" | xargs rm -f
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	${ROOT_DIR}/do-samples.sh hexchat
	@rm -f ${ROOT_DIR}/hexchat-raw.traces.gz

nmap: ## Generates traces for nmap and creates merged trace corpus.
	@echo "[lsee] Generating traces for nmap (SAMPLE=NONE)..."
	${ROOT_DIR}/lsee-all ../c2ocaml/artifacts/nmap ml4spec
	time make -f ./artifacts/Makefile.temp -j$$(getconf _NPROCESSORS_ONLN) all
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	@find ./artifacts/ -type f -name "nmap-*.traces" -print0 | xargs -0 -n1 -I'{}' bash -c "cat {} | gzip - >> ${ROOT_DIR}/nmap-raw.traces.gz"
	@find ./artifacts/ -type f -name "nmap-*" | xargs rm -f
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	${ROOT_DIR}/do-samples.sh nmap
	@rm -f ${ROOT_DIR}/nmap-raw.traces.gz

curl: ## Generates traces for curl and creates merged trace corpus.
	@echo "[lsee] Generating traces for curl (SAMPLE=NONE)..."
	${ROOT_DIR}/lsee-all ../c2ocaml/artifacts/curl ml4spec
	time make -f ./artifacts/Makefile.temp -j$$(getconf _NPROCESSORS_ONLN) all
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	@find ./artifacts/ -type f -name "curl-*.traces" -print0 | xargs -0 -n1 -I'{}' bash -c "cat {} | gzip - >> ${ROOT_DIR}/curl-raw.traces.gz"
	@find ./artifacts/ -type f -name "curl-*" | xargs rm -f
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	${ROOT_DIR}/do-samples.sh curl
	@rm -f ${ROOT_DIR}/curl-raw.traces.gz

linux: ## Generates traces for linux v4.5-rc4 and creates merged trace corpus.
	@echo "[lsee] Generating traces for linux (WARNING: VERY SLOW)..."
	@echo "[lsee] Using $$(getconf _NPROCESSORS_ONLN) workers..."
	${ROOT_DIR}/lsee-all ../c2ocaml/artifacts/linux
	time make -f ./artifacts/Makefile.temp -j$$(getconf _NPROCESSORS_ONLN) all
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	@echo "[lsee] Generated traces! Run 'NAME=linux make collect' to build a trace corpus."
