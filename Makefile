export WHERE := local
ifeq ($(WHERE),students)
	export BNFC := /home/students/inf/PUBLIC/MRJP/bin/students/bnfc
	export ALEX := alex
	export HAPPY := happy
else
	export BNFC := $(HOME)/.cabal/bin/bnfc
	export ALEX := $(HOME)/.cabal/bin/alex
	export HAPPY := $(HOME)/.cabal/bin/happy
endif

export PKGNAME := mateusz_dudzinski

.PHONY : all clean distclean

# Default goal.

all : TestLanguage

# Rules for building the parser.

ErrM.hs LexLanguage.x PrintLanguage.hs ParLanguage.y TestLanguage.hs : Language.cf
	$(BNFC) --haskell Language.cf

%.hs : %.y
	$(HAPPY) --ghc --coerce --array $<

%.hs : %.x
	$(ALEX) --ghc $<

TestLanguage : TestLanguage.hs ErrM.hs LexLanguage.hs ParLanguage.hs PrintLanguage.hs
	ghc --make $< -o $@

package : clean all
	-rm -rf $(PKGNAME)
	mkdir -p $(PKGNAME)
	cp Language.cf $(PKGNAME)/
	cp Makefile $(PKGNAME)/
	cp docs/README.txt $(PKGNAME)/
	cp docs/README.pdf $(PKGNAME)/
	zip -r $(PKGNAME).zip $(PKGNAME)
	-rm -rf $(PKGNAME)

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi *.x *.y
	-rm -f *.hs # for now.
	-rm -f DocLanguage.txt
	-rm -f TestLanguage
