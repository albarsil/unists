## Scrpit only used for specify options for running

options(showWarnCalls = TRUE)
options(showErrorCalls = TRUE)
options(show.error.messages = TRUE)
options(verbose = TRUE)
set.seed(23521)

options(scipen = 20) #remove e from print
options(digits = 5) #limit precision from print


cls <- function() cat(rep("\n",50))
