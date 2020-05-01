# use make language to generate all bnfc files.
# use make interpreter (or just make / make all) to generate interpreter program.

export WHERE := students
ifeq ($(WHERE),students)
	export BNFC := /home/students/inf/PUBLIC/MRJP/bin/students/bnfc
	export ALEX := alex
	export HAPPY := happy
	export GHC := ghc
else
	export BNFC := $(HOME)/.cabal/bin/bnfc
	export ALEX := $(HOME)/.cabal/bin/alex
	export HAPPY := $(HOME)/.cabal/bin/happy
	export GHC := ghc
endif

export PKGNAME := mateusz_dudzinski

.PHONY: all interpreter language clean package test validate
all: interpreter

language:
	$(BNFC) --functor --haskell Language.cf
	$(ALEX) --ghc LexLanguage.x
	$(HAPPY) --ghc --coerce --array ParLanguage.y

	@# Can't tell BNFC that I don't want these:
	-rm -f DocLanguage.txt PrintLanguage.hs SkelLanguage.hs TestLanguage.hs
	-mv -f AbsLanguage.hs ErrM.hs LexLanguage.hs ParLanguage.hs ./src

interpreter: src/Main.hs
	$(GHC) --make -Wall -isrc -isrc/bnfc src/Main.hs -odir obj -hidir obj -o $@

test:
	./interpreter < ./tests.txt

lint:
	hlint ./src/AbsLanguage.hs ./src/Common.hs ./src/Error.hs ./src/Eval.hs \
              ./src/Main.hs ./src/Parser.hs ./src/State.hs ./src/Static.hs

validate:
	@-cd ./test && ./validate.sh

package: clean all
	-rm -rf $(PKGNAME)
	mkdir -p $(PKGNAME)
	cp Language.cf $(PKGNAME)/
	cp Makefile $(PKGNAME)/
	cp docs/README.txt $(PKGNAME)/
	cp docs/README.pdf $(PKGNAME)/
	zip -r $(PKGNAME).zip $(PKGNAME)
	-rm -rf $(PKGNAME)

clean:
	-rm -f *.hi *.o *.log *.aux *.dvi *.x *.y *.out
	-rm -rf obj/ interf/
	-rm -f interpreter
