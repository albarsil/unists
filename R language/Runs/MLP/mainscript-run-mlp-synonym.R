setwd("git/")

RcppParallel::setThreadOptions(numThreads = 32)
source("Scrips/Commons/Config.R")
source("Scrips/Commons/utils.R")
source("Scrips/Propor/Database/DataSets.R")
source("Scrips/Commons/pre-processing.R")
source("Scrips/Commons/glove.R")
source("Scrips/Propor/Database/dbWordnet.R")
source("Scrips/Commons/similarity.R")
source("Scrips/Commons/visualize.R")
source("Scrips/Commons/graph_network.R")
source("Scrips/Propor/MachineLearning/validate.R")
source("Scrips/Propor/MachineLearning/machine_learning.R")

#sentences_train <- read.propor.dataset(filepath = "Data/Propor/assin-ptbr-train.xml", processing = FALSE)
#sentences_test <- read.propor.dataset(filepath = "Data/Propor/assin-ptbr-test.xml", processing = TRUE)


# Generate synonym
#sentence_train_synonym_1 <- apply(sentences_train[,1:2], 1, function(x) replace.sentence.synonym(x[1], x[2]))
#sentence_train_synonym_2 <- apply(sentences_train[,1:2], 1, function(x) replace.sentence.synonym(x[2], x[1]))

#sentence_test_synonym_1 <- apply(sentences_test[,1:2], 1, function(x) replace.sentence.synonym(x[1], x[2]))
#sentence_test_synonym_2 <- apply(sentences_test[,1:2], 1, function(x) replace.sentence.synonym(x[2], x[1]))

#sentences_train[,1] <- sentence_train_synonym_1
#sentences_train[,2] <- sentence_train_synonym_2

#sentences_test[,1] <- sentence_test_synonym_1
#sentences_test[,2] <- sentence_test_synonym_2

#write.table(sentences_train, "Data/Propor/assin-ptbr-train-synomy.csv", sep = ";", row.names = FALSE, col.names = TRUE, quote = TRUE, fileEncoding = "UTF-8")
#write.table(sentences_test, "Data/Propor/assin-ptbr-test-synomy.csv", sep = ";", row.names = FALSE, col.names = TRUE, quote = TRUE, fileEncoding = "UTF-8")

# Load synonym
require(data.table)
sentences_train <- fread("Data/Propor/assin-ptbr-train-synomy.csv", header = TRUE, sep = ";", encoding = "UTF-8", data.table = FALSE)
sentences_test <- fread("Data/Propor/assin-ptbr-test-synomy.csv", header = TRUE, sep = ";", encoding = "UTF-8", data.table = FALSE)

# read glove model
glove.model <- readRDS("Models/glove.pt")

print(0)

embeddings.train.vector.rowsums <- sim.embeddings.vector(sentences_train[,1:2], word_vectors = glove.model$get_word_vectors(), regularization = "none")
embeddings.test.vector.rowsums <- sim.embeddings.vector(sentences_test[,1:2], word_vectors = glove.model$get_word_vectors(), regularization = "none")

embeddings.train.pca.euclidean <- sim.pca.embeddings.matrix(sentences_train[,1:2], word_vectors = glove.model$get_word_vectors(), regularization = "none", checkDistance = TRUE, sim.method = "euclidean")
embeddings.test.pca.euclidean <- sim.pca.embeddings.matrix(sentences_test[,1:2], word_vectors = glove.model$get_word_vectors(), regularization = "none",checkDistance = TRUE, sim.method = "euclidean")

tfidf.train <- sim.tfidf(sentences_train[,1:2], regularization = "none")
tfidf.test <- sim.tfidf(sentences_test[,1:2], regularization = "none")

data.train <- data.frame(embeddings_sum = embeddings.train.vector.rowsums, embeddings_pca = embeddings.train.pca.euclidean, tfidf = tfidf.train)
data.test <- data.frame(embeddings_sum = embeddings.test.vector.rowsums, embeddings_pca = embeddings.test.pca.euclidean, tfidf = tfidf.test)

data.train.length1 <- vapply(sentences_train[,1], function(x) length(unlist(strsplit(x, " "))), FUN.VALUE = numeric(1))
data.train.length2 <- vapply(sentences_train[,2], function(x) length(unlist(strsplit(x, " "))), FUN.VALUE = numeric(1))
data.test.length1 <- vapply(as.character(sentences_test[,1]), function(x) length(unlist(strsplit(x, " "))), FUN.VALUE = numeric(1))
data.test.length2 <- vapply(as.character(sentences_test[,2]), function(x) length(unlist(strsplit(x, " "))), FUN.VALUE = numeric(1))

data.train.pca.pen <- embeddings.train.pca.euclidean + tfidf.train/ 2
data.test.pca.pen <- embeddings.test.pca.euclidean + tfidf.test/ 2

data.train.rowsums.pen <- embeddings.train.vector.rowsums + tfidf.train/ 2
data.test.rowsums.pen <- embeddings.test.vector.rowsums + tfidf.test/ 2

data.train$penality.rowsums <- sim.diff.penalization(sentences_train[,2], sentences_train[,3], data.train.rowsums.pen)
data.test$penality.rowsums <- sim.diff.penalization(as.character(sentences_test[,2]), as.character(sentences_test[,3]), data.test.rowsums.pen)

train.count.ngram <- apply(sentences_train[,1:2], 1, function(x) nrow(ngram(paste(x[1], x[2], collapse = " "), c(2,3), minOccurrences = 2)))
test.count.ngram <- apply(sentences_test[,1:2], 1, function(x) nrow(ngram(paste(x[1], x[2], collapse = " "), c(2,3), minOccurrences = 2)))

data.train$ngram_proportion <- train.count.ngram/(data.train.length1 + data.train.length2)
data.test$ngram_proportion <- test.count.ngram/(data.test.length1 + data.test.length2)

train.count.common_words <- apply(sentences_train[,1:2], 1, function(x) nrow(ngram(paste(x[1], x[2], collapse = " "), c(1,1), minOccurrences = 2)))
test.count.common_words <- apply(sentences_test[,1:2], 1, function(x) nrow(ngram(paste(x[1], x[2], collapse = " "), c(1,1), minOccurrences = 2)))

train.count.words <- apply(sentences_train[,1:2], 1, function(x) length(unlist(strsplit(paste(x[1], x[2], collapse = " "), " "))))
test.count.words <- apply(sentences_test[,1:2], 1, function(x) length(unlist(strsplit(paste(x[1], x[2], collapse = " "), " "))))

data.train$common_word_proportion <- train.count.common_words/(data.train.length1 + data.train.length2)
data.test$common_word_proportion <- test.count.common_words/(data.test.length1 + data.test.length2)

data.train$uncommon_word_proportion <- (train.count.words - train.count.common_words)/(data.train.length1 + data.train.length2)
data.test$uncommon_word_proportion <- (test.count.words - test.count.common_words)/(data.test.length1 + data.test.length2)

net <- pulo.network()
net.ant <- net$near_antonym()
net.ant <- distances(net.ant)

data.train$antonym <- count.antonym(net.ant, sentences_train[,1:2])
data.test$antonym <- count.antonym(net.ant, sentences_test[,1:2])

# MaxMin
#data.train <- apply(data.train, 2, function(x) (x-min(x))/(max(x)-min(x)))
#data.test <- apply(data.test, 2, function(x) (x-min(x))/(max(x)-min(x)))

# Z-score
#data.train <- apply(data.train, 2, function(x) (x-mean(x))/sd(x))
#data.test <- apply(data.test, 2, function(x) (x-mean(x))/sd(x))

data.train <- as.data.frame(data.train)
data.test <- as.data.frame(data.test)

data.train$target = as.numeric(as.character(sentences_train[,4]))
data.test$target = as.numeric(as.character(sentences_test[,4]))

# library(readr)
# library(jsonlite)
# write_lines(toJSON(data.train), "Datasets/mainscript-run-synonym-train.json")
# write_lines(toJSON(data.test), "Datasets/mainscript-run-synonym-test.json")

datalist = list()

lepochs <- c(500, 700, 1000)
lneurons <- c(5, 7, 10, 15, 19, 22, 30)
learningr <- c(0.01, 0.012, 0.12)

initTime <- Sys.time()
i <- 1
for(ep in lepochs){
  for (n in lneurons) {
    for(l in learningr){
      # ... make some data
      datalist[[i]] <- experiment.mlp(data.train, data.test, 1:ncol(data.train), epochs = ep, hidden_neurons = n, learning_rate = l)
      print(sprintf(" Interaction: %s - Epochs: %s - Neurons: %s - LR: %s", i, ep, n, l))
      i <- i + 1
    }
  }
}

print(0)

clearRunResults = do.call(rbind, datalist)
print(difftime(Sys.time(), initTime))

write.table(clearRunResults, "Results/mlp-run-results-synonym.csv", sep = ";", quote = TRUE, fileEncoding = "UTF-8", append = FALSE)