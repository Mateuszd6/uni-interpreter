# Makefile generated by BNFC.

# List of goals not corresponding to file names.

.PHONY : all clean distclean

# Default goal.

all : TestExp

# Rules for building the parser.

ErrM.hs LexExp.x PrintExp.hs ParExp.y TestExp.hs : Exp.cf
	~/.cabal/bin/bnfc --haskell Exp.cf

%.hs : %.y
	~/.cabal/bin/happy --ghc --coerce --array -d --info $<

%.hs : %.x
	~/.cabal/bin/alex --ghc $<

TestExp : TestExp.hs ErrM.hs LexExp.hs ParExp.hs PrintExp.hs
	ghc --make $< -o $@

# Rules for cleaning generated files.

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi

distclean : clean
	-rm -f AbsExp.hs AbsExp.hs.bak ComposOp.hs ComposOp.hs.bak DocExp.txt DocExp.txt.bak ErrM.hs ErrM.hs.bak LayoutExp.hs LayoutExp.hs.bak LexExp.x LexExp.x.bak ParExp.y ParExp.y.bak PrintExp.hs PrintExp.hs.bak SharedString.hs SharedString.hs.bak SkelExp.hs SkelExp.hs.bak TestExp.hs TestExp.hs.bak XMLExp.hs XMLExp.hs.bak ASTExp.agda ASTExp.agda.bak ParserExp.agda ParserExp.agda.bak IOLib.agda IOLib.agda.bak Main.agda Main.agda.bak Exp.dtd Exp.dtd.bak TestExp LexExp.hs ParExp.hs ParExp.info ParDataExp.hs Makefile


# EOF
