.SUFFIXES:

.ONESHELL:
.SHELLFLAGS += -e

CABAL = cabal
DOCTEST = doctest

ALL_DOCTEST_FLAGS = $(DOCTEST_FLAGS)

ifdef PACKAGE_DB
ALL_DOCTEST_FLAGS += -package-db=$(PACKAGE_DB) -package QuickCheck
else ifdef PACKAGE_ENV
ALL_DOCTEST_FLAGS += -package-env=$(PACKAGE_ENV)
endif

DOCTEST_SRC =
DOCTEST_SRC += src/Bits/ALaCarte.hs
DOCTEST_SRC += src/Bits/ApplicativeExamples.hs
DOCTEST_SRC += src/Bits/Bananas.hs
DOCTEST_SRC += src/Bits/FirstLenses.hs
DOCTEST_SRC += src/Bits/FunWithPhantomTypes.hs
DOCTEST_SRC += src/Bits/IteratorEssence.hs
DOCTEST_SRC += src/Bits/Origami.hs
DOCTEST_SRC += src/Bits/RankNTypesExamples.hs
DOCTEST_SRC += src/Bits/Singletons.hs
DOCTEST_SRC += src/Bits/Zipper.hs

all: build

build:
	$(CABAL) v2-build all

clean:
	$(CABAL) v2-clean

check test:
	@printf "Running doctests...\n"
	@for file in $(DOCTEST_SRC); do
	    printf "%s:\n" $$file;
	    $(DOCTEST) $$file $(ALL_DOCTEST_FLAGS);
	done

.PHONY: all build check clean test
