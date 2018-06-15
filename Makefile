SHELL=/bin/bash

ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

.PHONY: help
.PHONY: lsee
.PHONY: redis
.PHONY: nginx
.PHONY: hexchat
.PHONY: nmap
.PHONY: curl
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

redis: lsee ## Generates traces for redis and creates merged trace corpus.
	@echo "[lsee] Generating traces for redis..."
	@echo "[lsee] Using $$(nproc) workers..."
	${ROOT_DIR}/lsee-all ../c2ocaml/artifacts/redis
	time make -f ./artifacts/Makefile.temp -j$$(nproc) all
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	@echo "[lsee] Generated traces! Run 'NAME=redis make collect' to build a trace corpus."

nginx: lsee ## Generates traces for nginx and creates merged trace corpus.
	@echo "[lsee] Generating traces for nginx..."
	@echo "[lsee] Using $$(nproc) workers..."
	${ROOT_DIR}/lsee-all ../c2ocaml/artifacts/nginx
	time make -f ./artifacts/Makefile.temp -j$$(nproc) all
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	@echo "[lsee] Generated traces! Run 'NAME=nginx make collect' to build a trace corpus."

hexchat: lsee ## Generates traces for hexchat and creates merged trace corpus.
	@echo "[lsee] Generating traces for hexchat..."
	@echo "[lsee] Using $$(nproc) workers..."
	${ROOT_DIR}/lsee-all ../c2ocaml/artifacts/hexchat
	time make -f ./artifacts/Makefile.temp -j$$(nproc) all
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	@echo "[lsee] Generated traces! Run 'NAME=hexchat make collect' to build a trace corpus."

nmap: lsee ## Generates traces for nmap and creates merged trace corpus.
	@echo "[lsee] Generating traces for nmap..."
	@echo "[lsee] Using $$(nproc) workers..."
	${ROOT_DIR}/lsee-all ../c2ocaml/artifacts/nmap
	time make -f ./artifacts/Makefile.temp -j$$(nproc) all
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	@echo "[lsee] Generated traces! Run 'NAME=nmap make collect' to build a trace corpus."

curl: lsee ## Generates traces for curl and creates merged trace corpus.
	@echo "[lsee] Generating traces for curl..."
	@echo "[lsee] Using $$(nproc) workers..."
	${ROOT_DIR}/lsee-all ../c2ocaml/artifacts/curl
	time make -f ./artifacts/Makefile.temp -j$$(nproc) all
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	@echo "[lsee] Generated traces! Run 'NAME=curl make collect' to build a trace corpus."

linux: lsee ## Generates traces for linux v4.5-rc4 and creates merged trace corpus.
	@echo "[lsee] Generating traces for linux (WARNING: VERY SLOW)..."
	@echo "[lsee] Using $$(nproc) workers..."
	${ROOT_DIR}/lsee-all ../c2ocaml/artifacts/linux
	time make -f ./artifacts/Makefile.temp -j$$(nproc) all
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	@echo "[lsee] Generated traces! Run 'NAME=linux make collect' to build a trace corpus."

collect: ## REQUIRES PARAMETER 'NAME=<name>' : collects all traces in the artifacts directory into a trace corpus.
	@echo "[lsee] Creating trace corpus ${ROOT_DIR}/${NAME}.traces.txt"
	cat ${ROOT_DIR}/artifacts/*.traces > "${ROOT_DIR}/${NAME}.traces.txt"
	@echo "[lsee] Corpus created with $$(cat ${ROOT_DIR}/${NAME}.traces.txt | wc -l) traces."
	@rm -f ${ROOT_DIR}/artifacts/*.traces
	@rm -f ${ROOT_DIR}/artifacts/Makefile.temp
	@echo "[lsee] Cleaned up ${ROOT_DIR}/artifacts"
	@echo "[lsee] All done. Use GloVe to learn embeddings from your trace corpus!"
