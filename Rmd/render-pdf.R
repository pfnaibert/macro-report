#!/usr/bin/env Rscript

library(rmarkdown)
render("./00-gdp-report.Rmd", output_format="pdf_document",output_file="Report-2019-09")
cat(" \n ***** FIM DO SCRIPT ****** \n")
