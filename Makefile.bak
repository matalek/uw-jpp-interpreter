all:
	happy -gca ParMatal.y
	alex -g LexMatal.x
	latex DocMatal.tex; dvips DocMatal.dvi -o DocMatal.ps
	ghc --make TestMatal.hs -o TestMatal
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocMatal.ps
distclean: clean
	-rm -f DocMatal.* LexMatal.* ParMatal.* LayoutMatal.* SkelMatal.* PrintMatal.* TestMatal.* AbsMatal.* TestMatal ErrM.* SharedString.* Matal.dtd XMLMatal.* Makefile*

