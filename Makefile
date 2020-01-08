all:	ParLatte.hs latc_x86_64
	alex -g LexLatte.x
latc_x86_64:	lat_to_asm
	cp src/latc_x86_64 latc_x86_64

lat_to_asm:
	ghc --make -isrc src/latte.hs -o lat_to_asm
	
clean:
	rm -f lat_to_asm latc_x86_64
	rm -f *.log *.aux *.hi *.o *.dvi
	rm -f DocLatte.ps
	rm -f src/*.hi src/*.o
	rm -f ParLatte.hs ParLatte.y LexLatte.x DocLatte.txt
	rm -f Latte.cf ErrM.hs PrintLatte.hs TestLatte.hs TestLatte
	rm -f SkelLatte.hs LexLatte.hs AbsLatte.hs
	rm -f good/*.s
	rm -f ./*.dyn_hi
	rm -f ./*.dyn_o
	rm -f ./src/*.dyn_hi
	rm -f ./src/*.dyn_o


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
