#!/usr/bin/Rscript
require(knitr)
require(markdown)
require(rmarkdown)

arg.vec <- commandArgs(trailingOnly=TRUE)
for (fn in arg.vec) {
    render(fn)
}
