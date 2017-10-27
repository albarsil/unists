


GetPythagorasDistance <- function(sentences, word_vectors = WordVectors){
  require(stats)
  
  my_formula <- function(frase1, frase2){
    
    resultVector <- list()
    for(i in 1:nrow(x)){
      resultVector[[i]] <- ((x[i,1] - y[i,1]) ^ 2) + ((x[i,2] - y[i,2]) ^ 2)
    }
    
    res <- sqrt(sum(unlist(resultVector)))
    return(res)
  }
  
  local_pca_sim_we_sentences <-c()
  
  for(index in 1:nrow(sentences)){
    
    x <- as.embeddings.matrix(sentences[index,1], word_vectors)
    y <- as.embeddings.matrix(sentences[index,2], word_vectors)
    
    x <- princomp(x, cor = FALSE)
    y <- princomp(y, cor = FALSE)
    
    x <- as.matrix(x$scores) #Utilizar apenas o Score, a PCA 1
    y <- as.matrix(y$scores) #Utilizar apenas o Score, a PCA 1
    
    local_pca_sim_we_sentences <- rbind(local_pca_sim_we_sentences, my_formula(x[,1:2], y[,1:2]))
  }
  
  return(local_pca_sim_we_sentences)
}

w1 <- GetPythagorasDistance(sentences_train)
w2 <- GetPythagorasDistance(sentences_test)
