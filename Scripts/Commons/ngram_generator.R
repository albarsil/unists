
library(doMC)
registerDoMC(40)

source("~/git/Scrips/Commons/Config.R")
source("~/git/Scrips/Commons/utils.R")
source("~/git/Scrips/Propor/Database/DataSets.R")
source("~/git/Scrips/Commons/pre-processing.R")
source("~/git/Scrips/Commons/glove.R")


args <- commandArgs(trailingOnly=TRUE)

if(length(args) == 0){
  print("Sao necessarios dois parametros: [1] O caminho da pasta de origem [2] O caminho do arquivo de destino")
  q()
}

fromPath <- args[1]
destFile <- args[2]

files.dir <- list.files(path = fromPath)

library(readr)
initialTime <- Sys.time()

print("Start searching for ngrams(2,3) with minimum of 2 occurrences")

for(f in files.dir){
  glove.corpus <- read_lines(paste(fromPath, f, sep = "/"))
  a <- ngram(glove.corpus, c(2, 3), minOccurrences = 1)
  write.table(a, destFile, sep = ";", row.names = FALSE, col.names = FALSE, fileEncoding = "UTF-8", append = TRUE)
}

print(difftime(Sys.time(), initialTime))

