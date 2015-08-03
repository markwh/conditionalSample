# main.r

library(roxygen2)
library(devtools)

# to make my code look pretty
install.packages("formatR")
formatR::tidy_dir("R")


# To make Roxygen documentation:
devtools::document(".")

# To load the package:
load_all("../conditionalSample/", reset = FALSE)

# Checks?
# check_doc()

# To install the package:
devtools::install("../conditionalSample/")

library(conditionalSample)
?condlSample
