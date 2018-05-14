
FIGURES=\
  Figures/Architecture.tex \
  Figures/CubeSat-1.eps    \
  Figures/Tool-Chain.tex   \

SOURCES=\
  conclusion.tex            \
  introduction.tex          \
  main.tex                  \
  project-description.tex   \
  student-participation.tex \
  system-overview.tex

main.pdf: $(SOURCES) $(FIGURES)
	latex main.tex
	bibtex main
	latex main.tex
	latex main.tex
	dvips main
	ps2pdf main.ps main.pdf

clean:
	rm -f main.pdf main.aux main.bbl main.blg main.dvi main.log main.ps
