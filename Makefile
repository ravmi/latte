all:
	bnfc Latte.cf
	happy -gca ParLatte.y
	alex -g LexLatte.x
	ghc --make TestLatte.hs -o TestLatte
	ghc --make latte.hs Asm64.hs -o lat_to_asm

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi *.x *.hi *.o AbsCodi.hs DocCodi.hs  ErrM.hs  PrintCodi.hs ParCodi.hs SkelCodi.hs TestCodi.hs DocCodi.txt LexCodi.hs ParCodi.y TestCodi interpreter *.bak

distclean: clean
	-rm -f DocCodi.* LexCodi.* ParCodi.* LayoutCodi.* SkelCodi.* PrintCodi.* TestCodi.* AbsCodi.* TestCodi ErrM.* SharedString.* ComposOp.* codi.dtd XMLCodi.* Makefile*
	

