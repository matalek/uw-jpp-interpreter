all:
	happy -gca Parmatal.y
	alex -g Lexmatal.x
	latex Docmatal.tex; dvips Docmatal.dvi -o Docmatal.ps
	ghc --make Testmatal.hs -o Testmatal
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f Docmatal.ps
distclean: clean
	-rm -f Docmatal.* Lexmatal.* Parmatal.* Layoutmatal.* Skelmatal.* Printmatal.* Testmatal.* Absmatal.* Testmatal ErrM.* SharedString.* matal.dtd XMLmatal.* Makefile*

