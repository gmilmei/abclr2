ROOT := $(abspath ..)
BUILD := $(ROOT)/build/tutorial-build
export TEXMFVAR := $(BUILD)/texmf-var
export TEXMFCACHE := $(BUILD)/texmf-var
LUALATEX ?= lualatex
SBCL ?= sbcl
TEST_RUNNER := $(SBCL) --script tutorial/tests/run-tests.lisp

.PHONY: all pdf check runner-check coverage-check baseline parser-check semantics-check reading-check reflection-check guards-check heavy-old-check multicast-check copy-check watch-check report-check host-events-check loading-check pcontrol-check layout-check timewarp-check timewarp-variants-check retry-check break-check express-check interrupt-check
all: pdf

pdf:
	mkdir -p "$(BUILD)" "$(TEXMFVAR)"
	cd "$(ROOT)" && $(SBCL) --script tutorial/prepare-listings.lisp "$(BUILD)"
	cd "$(ROOT)" && $(LUALATEX) -interaction=nonstopmode -halt-on-error -output-directory "$(BUILD)" tutorial/tutorial.tex
	cd "$(ROOT)" && $(LUALATEX) -interaction=nonstopmode -halt-on-error -output-directory "$(BUILD)" tutorial/tutorial.tex
	cd "$(ROOT)" && $(LUALATEX) -interaction=nonstopmode -halt-on-error -output-directory "$(BUILD)" tutorial/tutorial.tex
	cp "$(BUILD)/tutorial.pdf" "$(ROOT)/doc/tutorial.pdf"

baseline:
	cd "$(ROOT)" && $(TEST_RUNNER) original-tests

runner-check:
	cd "$(ROOT)" && $(TEST_RUNNER) --self-check

coverage-check:
	cd "$(ROOT)" && $(SBCL) --script tutorial/tests/check-source-map.lisp

parser-check:
	cd "$(ROOT)" && $(TEST_RUNNER) parser-tests

semantics-check:
	cd "$(ROOT)" && $(TEST_RUNNER) tutorial-semantics

reading-check:
	cd "$(ROOT)" && $(TEST_RUNNER) reading-tests

reflection-check:
	cd "$(ROOT)" && $(TEST_RUNNER) reflection-tests

guards-check:
	cd "$(ROOT)" && $(TEST_RUNNER) guard-tests

heavy-old-check:
	cd "$(ROOT)" && $(TEST_RUNNER) heavy-old-tests

multicast-check:
	cd "$(ROOT)" && $(TEST_RUNNER) multicast-tests

copy-check:
	cd "$(ROOT)" && $(TEST_RUNNER) copy-tests

watch-check:
	cd "$(ROOT)" && $(TEST_RUNNER) watch-tests

report-check:
	cd "$(ROOT)" && $(TEST_RUNNER) report-tests

host-events-check:
	cd "$(ROOT)" && $(TEST_RUNNER) host-event-tests

loading-check:
	cd "$(ROOT)" && $(TEST_RUNNER) loading-tests

pcontrol-check:
	cd "$(ROOT)" && $(TEST_RUNNER) pcontrol-tests

layout-check:
	mkdir -p "$(ROOT)/build/listing-tests"
	cd "$(ROOT)" && $(SBCL) --script tutorial/prepare-listings.lisp "$(ROOT)/build/listing-tests" --check

timewarp-check:
	cd "$(ROOT)" && $(TEST_RUNNER) timewarp-tests

timewarp-variants-check:
	cd "$(ROOT)" && $(TEST_RUNNER) timewarp-variants

retry-check:
	cd "$(ROOT)" && $(TEST_RUNNER) retry-tests

break-check:
	cd "$(ROOT)" && $(TEST_RUNNER) break-tests

express-check:
	cd "$(ROOT)" && $(TEST_RUNNER) express-tests

interrupt-check:
	cd "$(ROOT)" && $(TEST_RUNNER) interrupt-tests

.PHONY: serialization-check
serialization-check:
	cd "$(ROOT)" && $(TEST_RUNNER) serialization-tests

.PHONY: group-check
group-check:
	cd "$(ROOT)" && $(TEST_RUNNER) group-tests

.PHONY: source-views-check
source-views-check:
	cd "$(ROOT)" && $(SBCL) --script tutorial/tests/check-source-views.lisp

check: source-views-check runner-check coverage-check baseline parser-check serialization-check semantics-check reading-check reflection-check group-check guards-check multicast-check copy-check watch-check report-check host-events-check loading-check pcontrol-check timewarp-check timewarp-variants-check retry-check break-check express-check interrupt-check
	cd "$(ROOT)" && $(TEST_RUNNER) tutorial-tests
