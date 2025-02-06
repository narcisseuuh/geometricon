#import "style/style.typ": temp
#import "logo.typ": LaTeX, LaTeX as LaTeX2e

#let affls = (
  tsp: (
    department: "Department of Computer Science",
    institution: "Télécom SudParis"),
)

#let authors = (
  (name: "Augustin Perrin",
   email: "augustin.perrin@telecom-sudparis.eu",
   affl: ("tsp")),
)

#show: temp.with(
  title: [Implementing an Abstract Analyzer and Interpreter\
   for Geometricon],
  authors: (authors, affls),
  keywords: ("abstract interpretation"),
  abstract: [
    This project involves the implementation of a basic analyzer and interpreter for a geometric language in $RR^2$, inspired by the book "Introduction to Static Analysis" by Xavier Rival and Kwangyeuk Yi. The focus is on applying abstract interpretation techniques as detailed in the book.
  ],
  bibliography: bibliography("main.bib"),
  appendix: include "appendix.typ",
)

#let url(uri) = {
  link(uri, raw(uri))
}

