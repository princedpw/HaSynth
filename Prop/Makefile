all:	proposal.pdf

TEXFILES = $(wildcard *.tex)
BIBFILES = $(wildcard *.bib)
STYFILES = $(wildcard *.sty)

proposal: ${TEXFILES} ${BIBFILES} ${STYFILES}
	pdflatex proposal
	bibtex proposal
	pdflatex proposal
	pdflatex proposal

summary: ${TEXFILES} ${STYFILES}
	pdflatex summary

xelatex: ${TEXFILES} ${BIBFILES} ${STYFILES}
	xelatex proposal
	-bibtex proposal
	xelatex proposal
	xelatex proposal

once:
	pdflatex proposal



proof: proposal.tex ${TEXFILES}
	echo "weasel words: "
	sh bin/weasel proposal.tex
	echo
	echo "passive voice: "
	sh bin/passive proposal.tex
	echo
	echo "duplicates: "
	perl bin/dups proposal.tex

dpw:
	pdflatex dpw-bio

millstein:
	pdflatex millstein-bio

data:
	pdflatex data

collaboration:
	pdflatex collaboration

clean:
	rm -f *.aux *.bbl *.blg *.log *.dvi *.bak *~ proposal.ps proposal.pdf 
