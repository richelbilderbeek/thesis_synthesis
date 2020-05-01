all: synthesis.pdf

synthesis.pdf: *.tex
	./create.sh
	./view.sh

