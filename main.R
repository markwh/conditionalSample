# main.r

library(roxygen2)
library(devtools)

# To make Roxygen documentation:
devtools::document()

# To load the package:
load_all("../conditionalSample/")

# Checks?
check_doc()

# To install the package:
devtools::install("../conditionalSample/")
library(conditionalSample)
?condlSample
