#!/bin/sh

cat README.tex |\
    sed 's/\\begin{verbatim}/\\begin{minted}{cpp}/g' |\
    sed 's/\\end{verbatim}/\\end{minted}/g' >\
        README2.tex

mv README2.tex README.tex
pdflatex --shell-escape README.tex
