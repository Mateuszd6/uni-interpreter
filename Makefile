# use make language to generate all bnfc files.
# use make interpreter (or just make / make all) to generate interpreter program.

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

export BNFC_TARGETS := LexLanguage.hs ParLanguage.hs AbsLanguage.hs SkelLanguage.hs PrintLanguage.hs ErrM.hs

export PKGNAME := mateusz_dudzinski

.PHONY : all interpreter language clean package
all : interpreter

LexLanguage.x ParLanguage.y : Language.cf
	$(BNFC) --haskell Language.cf
	-@rm DocLanguage.txt # Can't tell BNFC that I don't want it

%.hs : %.y
	$(HAPPY) --ghc --coerce --array $<

%.hs : %.x
	$(ALEX) --ghc $<

language : Language.cf $(BNFC_TARGETS)

interpreter : ./src/Main.hs
	ghc --make -isrc src/Main.hs -odir obj -hidir interf -o $@

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
	-rm -f *.hs
	-rm -rf obj/ interf/
	-rm -f DocLanguage.txt
	-rm -f TestLanguage
