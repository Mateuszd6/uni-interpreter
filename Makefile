# Makefile generated by BNFC was so exceptionally bad I had to rewerite it by
# hand. Luckily BNFC does not generate it when not asked.

# List of goals not corresponding to file names.

export BNFC := $(HOME)/.cabal/bin/bnfc
export ALEX := $(HOME)/.cabal/bin/alex
export HAPPY := $(HOME)/.cabal/bin/happy


.PHONY : all clean distclean

# Default goal.

all : TestExp

# Rules for building the parser.

ErrM.hs LexExp.x PrintExp.hs ParExp.y TestExp.hs : Exp.cf
	$(BNFC) --haskell Exp.cf

%.hs : %.y
	$(HAPPY) --ghc --coerce --array $<

%.hs : %.x
	$(ALEX) --ghc $<

TestExp : TestExp.hs ErrM.hs LexExp.hs ParExp.hs PrintExp.hs
	ghc --make $< -o $@

# Rules for cleaning generated files.

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi

distclean : clean
	-rm -f AbsExp.hs AbsExp.hs.bak ComposOp.hs ComposOp.hs.bak DocExp.txt DocExp.txt.bak ErrM.hs ErrM.hs.bak LayoutExp.hs LayoutExp.hs.bak LexExp.x LexExp.x.bak ParExp.y ParExp.y.bak PrintExp.hs PrintExp.hs.bak SharedString.hs SharedString.hs.bak SkelExp.hs SkelExp.hs.bak TestExp.hs TestExp.hs.bak XMLExp.hs XMLExp.hs.bak ASTExp.agda ASTExp.agda.bak ParserExp.agda ParserExp.agda.bak IOLib.agda IOLib.agda.bak Main.agda Main.agda.bak Exp.dtd Exp.dtd.bak TestExp LexExp.hs ParExp.hs ParExp.info ParDataExp.hs Makefile


# EOF
