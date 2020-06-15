#!/usr/bin/env Rscript

library(rmarkdown)
render("./01-report.Rmd", output_file="Report-2019-09")
cat(" \n ***** FIM DO SCRIPT ****** \n")
