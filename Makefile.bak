all:
	happy -gca ParMatal.y
	alex -g LexMatal.x
	ghc --make TestMatal.hs -o TestMatal
	ghc --make Interpret.hs -o Interpret
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi Interpret TestMatal

distclean: clean
	-rm -f DocMatal.* LexMatal.* ParMatal.* LayoutMatal.* SkelMatal.* PrintMatal.* TestMatal.* AbsMatal.* TestMatal ErrM.* SharedString.* ComposOp.* Matal.dtd XMLMatal.* Makefile*
	

