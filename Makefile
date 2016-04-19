all:
	happy -gca ParMatal.y
	alex -g LexMatal.x
	ghc --make TestMatal.hs -o TestMatal

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocMatal.* LexMatal.* ParMatal.* LayoutMatal.* SkelMatal.* PrintMatal.* TestMatal.* AbsMatal.* TestMatal ErrM.* SharedString.* ComposOp.* Matal.dtd XMLMatal.* Makefile*
	

