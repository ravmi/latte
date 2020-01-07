all:	ParLatte.hs latc_x86_64
	alex -g LexLatte.x
insc_jvm:	latc
	cp src/latc_x86_64 latc_x86_64

comp_jvm:
	ghc --make -isrc src/latte.hs -o latte
	
clean:
	rm -f latte latc_x86_64
	rm -f *.log *.aux *.hi *.o *.dvi
	rm -f DocLatte.ps
	rm -f src/*.hi src/*.o
	rm -f ParLatte.hs ParLatte.y LexLatte.x DocLatte.txt
	rm -f Latte.cf ErrM.hs PrintLatte.hs TestLatte.hs TestLatte
	rm -f SkelLatte.hs LexLatte.hs AbsLatte.hs


ParLatte.y:	Latte.cf
	alex -g LexLatte.x
Latte.cf:
	cp src/Latte.cf .
	cp src/AbsLatte.hs .
	cp src/ErrM.hs .
	cp src/LexLatte.x .
	cp src/ParLatte.y .
	cp src/PrintLatte.hs .
	cp src/SkelLatte.hs .

ParLatte.hs:	ParLatte.y
	happy -gca $<
