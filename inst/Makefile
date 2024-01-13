all:
	R CMD Sweave freegroup_arxiv.Rnw
	pdflatex freegroup_arxiv
	bibtex freegroup_arxiv
	pdflatex freegroup_arxiv
	pdflatex freegroup_arxiv

clean:
	rm -f  *.aux *.log *.out *.bbl *.blg *.Rout freegroup_arxiv.tex

