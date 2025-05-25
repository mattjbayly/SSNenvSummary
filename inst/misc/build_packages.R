#----------------------------------------------------
# Build and Tests
#----------------------------------------------------

library(usethis)
library(testthat)
library(rhub)
library(devtools)
library(qpdf)
library(kableExtra)
library(testthat)

# Loading unfinished package to memory...
rm(list = ls()); getwd()
devtools::load_all()
devtools::document()
devtools::test()  # Run tests
devtools::check() # Operating system test

remove.packages("SSNenvSummary")
install.packages(getwd(), repos = NULL, type = "source")


#
##
###

# use_mit_license()
