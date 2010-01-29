all:	pdf html

html:	doc.html

pdf:	doc.pdf

clean:
	rm -f *.aux
	rm -f *.haux
	rm -f *.htoc
	rm -f *.idx
	rm -f *.ilg
	rm -f *.ind
	rm -f *.log
	rm -f *.out
	rm -f *.toc
	rm -f *.pdf

distclean: clean
	rm -f *.tex

doc.html:	doc.tex
		hevea -fix -pedantic doc.tex

doc.pdf:	doc.tex
		pdflatex doc.tex
		pdflatex doc.tex
