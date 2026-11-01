# LeanMail: the targets a person or CI runs. Everything else is under lean/, worker/, server/.

LEAN_DIR := lean
WORKER_DIR := worker
APP_DIR := app
# LeanNative's scripts, from the app's pinned elmlean package
ELMLEAN_CI := $(APP_DIR)/.lake/packages/elmlean/framework/native/ci
APP_ENV := APP_DIR=$(APP_DIR) APP_ROOT=LeanMailApp SCHEME=LeanMailApp

.PHONY: build test core-test address-test ical-test junk-test server-test spec spec-check accept server cli cli-smoke worker-install worker-dev worker-typecheck toolchain-cache docker dev clean app app-ios app-catalyst app-ui-tests

# The Linux Lean toolchain the image builds with, fetched once on the host. The ~600 MB download
# keeps dropping inside an emulated build stage, so it is resumed here and COPYed into the image.
LEAN_VERSION := $(patsubst leanprover/lean4:v%,%,$(shell cat $(LEAN_DIR)/lean-toolchain))
TOOLCHAIN_TAR := server/.cache/lean-$(LEAN_VERSION)-linux.tar.zst
TOOLCHAIN_URL := https://releases.lean-lang.org/lean4/v$(LEAN_VERSION)/lean-$(LEAN_VERSION)-linux.tar.zst
# Litestream, cached the same way (keep in step with LITESTREAM_VERSION in server/Dockerfile).
LITESTREAM_VERSION := 0.5.17
LITESTREAM_TAR := server/.cache/litestream-$(LITESTREAM_VERSION)-linux-x86_64.tar.gz
LITESTREAM_URL := https://github.com/benbjohnson/litestream/releases/download/v$(LITESTREAM_VERSION)/litestream-$(LITESTREAM_VERSION)-linux-x86_64.tar.gz

build:            ## build every Lean target
	cd $(LEAN_DIR) && lake build

test: core-test address-test ical-test junk-test server-test  ## run the Lean test executables

core-test:
	cd $(LEAN_DIR) && lake build core-test && ./.lake/build/bin/core-test

address-test:
	cd $(LEAN_DIR) && lake build address-test && ./.lake/build/bin/address-test

ical-test:
	cd $(LEAN_DIR) && lake build ical-test && ./.lake/build/bin/ical-test

junk-test:
	cd $(LEAN_DIR) && lake build junk-test && ./.lake/build/bin/junk-test

server-test:
	cd $(LEAN_DIR) && lake build server-test && ./.lake/build/bin/server-test

spec:             ## run every suite (Lean, Worker, cli-smoke) and regenerate docs/SPEC.md from the passing names
	./scripts/spec.sh

spec-check:       ## fail if docs/SPEC.md is not what the suites print now (CI)
	./scripts/spec.sh --check

accept: toolchain-cache  ## local end to end: image, wrangler dev, /health, /internal/health, a delivery, leanmail search
	./scripts/accept.sh

server:           ## run the Lean server bare on :8080 with ./data
	cd $(LEAN_DIR) && lake build leanmail-server && DATA_DIR=../data ./.lake/build/bin/leanmail-server

cli:              ## build the leanmail CLI and print its version
	cd $(LEAN_DIR) && lake build leanmail && ./.lake/build/bin/leanmail version

cli-smoke:        ## the CLI against a bare local server: login, import the fixtures, read them back
	cd $(LEAN_DIR) && lake build leanmail leanmail-server
	./scripts/smoke-cli.sh

app:              ## the native app's Lean (Core included), its proof gate and its selftest
	cd $(APP_DIR) && lake build && lake build leanmail-app-cli && ./.lake/build/bin/leanmail-app-cli selftest

app-ios: app      ## the app for the iOS simulator (Debug)
	$(APP_ENV) $(ELMLEAN_CI)/build-ios Debug iossim

app-catalyst: app ## the Mac Catalyst app (Debug); needs LEAN_X86_64_PREFIX (framework/native/scripts/fetch_lean_x86_64.sh)
	$(APP_ENV) $(ELMLEAN_CI)/build-ios Debug maccatalyst

app-ui-tests: app ## the UI tests on a throwaway simulator against a bare dev server with the fixture mail
	UI_BEFORE="$(CURDIR)/$(APP_DIR)/ios/ui-before" UI_AFTER="$(CURDIR)/$(APP_DIR)/ios/ui-after" $(APP_ENV) $(ELMLEAN_CI)/ui-tests

worker-install:
	cd $(WORKER_DIR) && npm install

worker-typecheck:
	cd $(WORKER_DIR) && npm run typecheck

toolchain-cache: $(TOOLCHAIN_TAR) $(LITESTREAM_TAR)  ## fetch the Linux Lean toolchain and Litestream for the image (resumable)

$(TOOLCHAIN_TAR):
	@mkdir -p $(@D)
	@i=0; until zstd -tq $@.part 2>/dev/null; do \
	  i=$$((i+1)); [ $$i -le 200 ] || { echo "toolchain download failed"; exit 1; }; \
	  curl -fL -C - --speed-time 30 --speed-limit 1024 -sS -o $@.part $(TOOLCHAIN_URL) \
	    || { echo "toolchain download interrupted (attempt $$i), resuming"; sleep 2; }; \
	done
	@mv $@.part $@ && ls -l $@

$(LITESTREAM_TAR):
	@mkdir -p $(@D)
	@i=0; until gzip -tq $@.part 2>/dev/null; do \
	  i=$$((i+1)); [ $$i -le 50 ] || { echo "litestream download failed"; exit 1; }; \
	  curl -fL -C - --speed-time 30 --speed-limit 1024 -sS -o $@.part $(LITESTREAM_URL) \
	    || { echo "litestream download interrupted (attempt $$i), resuming"; sleep 2; }; \
	done
	@mv $@.part $@ && ls -l $@

docker: toolchain-cache  ## build the container image the Worker deploys
	docker build --platform linux/amd64 -f server/Dockerfile -t leanmail-server:dev .

worker-dev: toolchain-cache  ## wrangler dev; it builds the container image from server/Dockerfile itself
	cd $(WORKER_DIR) && npx wrangler dev

dev: docker worker-dev

clean:
	rm -rf $(LEAN_DIR)/.lake $(APP_DIR)/.lake $(APP_DIR)/ios/build $(WORKER_DIR)/node_modules $(WORKER_DIR)/.wrangler
