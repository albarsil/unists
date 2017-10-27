
initialTime <- Sys.time()
require(scales)

size = 1000
split = 60
rescale_result = TRUE

sentences_train <- read.propor(filepath = "Data/assin-ptbr-train.xml", processing = TRUE)

if(size != 0 && split != 0){
  split <- split/100
  split <- as.integer(nrow(sentences_train) * split)
  
  sentences_test <- sentences_train[(split + 1):nrow(sentences_train),]
  sentences_train <- sentences_train[1:split,]
}

# Embeddings
train_set <- data.frame(as.numeric(GetCosineDistanceBetweenSentences(sentences_train[,1:2])))
test_set <- data.frame(as.numeric(GetCosineDistanceBetweenSentences(sentences_test[,1:2])))

# TF-IDF
train_set <- cbind(
  train_set, 
  as.numeric(
    PairSentenceToConsineDistance(
      GetPairSentenceTfIdf(sentences_train[,1:2])
    )
  )
)
test_set <- cbind(
  train_set, 
  as.numeric(
    PairSentenceToConsineDistance(
      GetPairSentenceTfIdf(sentences_test[,1:2])
    )
  )
)

# PCA
train_set <- cbind(
  train_set, 
  as.numeric(GetCosinePCABetweenEmbeddings(sentences_train[,1:2]))
)
test_set <- cbind(
  train_set, 
  as.numeric(GetCosinePCABetweenEmbeddings(sentences_test[,1:2]))
)

colnames(train_set) <- c("embeddings", "tfidf", "pca", "target")
colnames(test_set) <- c("embeddings", "tfidf", "pca", "target")

if(rescale_result){
  for(index in 1:ncol(train_set)){
    train_set[,index] <- rescale(train_set[,index], c(-1, 1))
    test_set[,index] <- rescale(test_set[,index], c(-1, 1))
  }
}

train_set <- cbind(train_set, as.numeric(sentences_train[,4]))
test_set <- cbind(test_set, as.numeric(sentences_test[,4]))


# Avereage word vectors results
exp1.results <- AvereageResults(target ~ embeddings, train_set[,c(1, 4)], test_set[,c(1, 4)])

exp1.results <- rbind(exp1.results, AvereageResults(target ~ pca, train_set[,c(3, 4)], test_set[,c(3, 4)]))

# Avereage Tf Idf results
exp1.results <- rbind(exp1.results, AvereageResults(target ~ tfidf, train_set[,c(2, 4)], test_set[,c(2, 4)]))

# Avereage Pca and TF-Idf results
exp1.results <- rbind(exp1.results, AvereageResults(target ~ pca + tfidf, train_set[,-1], test_set[,-1]))

# Avereage word vectors and TF Idf results
exp1.results <- rbind(exp1.results, AvereageResults(target ~ embeddings + tfidf, train_set[,-3], test_set[,-3]))

# Avereage PCA, word vectors and TF Idf results
exp1.results <- rbind(exp1.results, AvereageResults(target ~ pca + embeddings, train_set[,-2], test_set[,-2]))

# Avereage PCA, word vectors and TF Idf results
exp1.results <- rbind(exp1.results, AvereageResults(target ~ pca + embeddings + tfidf, train_set, test_set))

colnames(exp1.results) <- c("Correlation", "EQM")
rownames(exp1.results) <- c("Embeddings", "PCA", "Tf-Idf", "PCA + Tf-Idf", "Embeddings + Tf-Idf", "PCA + Embeddings", "PCA + Embeddings + Tf-Idf")
print(exp1.results)



train.pos <- 
a <- POSTag(sentences_train[,1])

print(difftime(Sys.time(), initialTime))