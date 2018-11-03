
as.tfidf <- function(sentences, ngram_bounduary = c(1L, 1L), token_min = 3L, skip_gram_window = 5L){
  require(text2vec)
  
  local_tfidf <- TfIdf$new()
  
  get_sentence_tfidf <- function(sentence_pair){
    if(is.character(sentence_pair) == FALSE){
      sentence_pair <- as.character(sentence_pair)
    }
    
    local_sentence_token <- itoken(sentence_pair, tokenizer = space_tokenizer, progressbar = FALSE)
    local_sentence_vocab = create_vocabulary(local_sentence_token)
    local_sentence_vectorizer = vocab_vectorizer(local_sentence_vocab)
    local_dtm <- create_dtm(local_sentence_token, local_sentence_vectorizer)
    
    local_tfidf_matrix <- fit_transform(local_dtm, local_tfidf)
    local_tfidf_matrix <- as.matrix(local_tfidf_matrix)
    
    local_tfidf_matrix <- data.frame(
      t = paste(as.character(local_tfidf_matrix[1,]), collapse=";"), 
      h = paste(as.character(local_tfidf_matrix[2,]), collapse=";") 
    )  
    
    return(local_tfidf_matrix)
  }
  
  local_list_tfidf <- c()
  
  for(index in 1:nrow(sentences)){
    row <- sentences[index, ]
    row[1] <- Stemming(row[1])
    row[2] <- Stemming(row[2])
    
    row <- get_sentence_tfidf(row)
    
    local_list_tfidf <- rbind(local_list_tfidf, row)
  }
  
  return(local_list_tfidf)
}

as.embeddings.vector <- function(sentence, word_vectors, sumFunction = rowSums){
  embeddings_phrase <- as.embeddings.matrix(sentence, word_vectors)
  embeddings_phrase <- as.matrix(sumFunction(embeddings_phrase))
  
  return(embeddings_phrase)
}

as.embeddings.matrix <- function(sentence, word_vectors){
  splited_phrase <- unlist(strsplit(as.character(trimws(sentence, which = "both")), " "))
  splited_phrase <- unlist(lapply(splited_phrase, tolower))
  
  get_embeddings <- function(el){
    if(el %in% rownames(word_vectors)){
      el <- word_vectors[el, , drop = FALSE]
    }
    else{
      el <- matrix(0, ncol(word_vectors))
    }
    return(as.numeric(el))
  }
  
  splited_phrase <- sapply(splited_phrase, get_embeddings)
  columns_only_zero <- apply(splited_phrase, 2, function(x) mean(x) == 0)
  
  mean_phrase <- apply(splited_phrase, 1, mean)
  
  splited_phrase[,which(columns_only_zero == TRUE)] <- mean_phrase
  return(splited_phrase)
}

sim.embeddings.vector <- function(sentences, word_vectors = WordVectors, sim.method = "cosine", regularization = "none"){
  local_we_sentences_t <- sapply(sentences[, 1], function(x) as.embeddings.vector(x, word_vectors, sumFunction = rowSums)) # It is a list of lists of word embeddings
  local_we_sentences_h <- sapply(sentences[, 2], function(x) as.embeddings.vector(x, word_vectors, sumFunction = rowSums))
  
  local_cos_sim_we_sentences <-1:nrow(sentences)
  
  calculate.similarity_ <- function(index){
    return(sim2(t(as.matrix(local_we_sentences_t[,index])), t(as.matrix(local_we_sentences_h[,index])), method = sim.method, norm = regularization))
  }
  
  local_cos_sim_we_sentences <- sapply(local_cos_sim_we_sentences, function(x) calculate.similarity_(x))
  
  return(local_cos_sim_we_sentences)
}

sim.pca.embeddings.matrix <- function(sentences, word_vectors = WordVectors, sim.method = "cosine", checkDistance = FALSE, regularization = "none"){
  require(stats)
  
  local_pca_cos_sim_we_sentences <- 1:nrow(sentences)
  
  calculate.similarity_ <- function(index){
    x <- as.embeddings.matrix(sentences[index,1], word_vectors)
    y <- as.embeddings.matrix(sentences[index,2], word_vectors)
    
    x <- princomp(x, cor = FALSE)
    y <- princomp(y, cor = FALSE)
    
    x <- as.matrix(x$scores) #Utilizar apenas o Score, a PCA 1
    y <- as.matrix(y$scores) #Utilizar apenas o Score, a PCA 1
    
    if(checkDistance){
      return(dist2(t(x[,1]), t(y[,1]), method = sim.method, norm = regularization))
    }else{
      return(sim2(t(x[,1]), t(y[,1]), method = sim.method, norm = regularization))
    }
  }
  
  local_pca_cos_sim_we_sentences <- unlist(lapply(local_pca_cos_sim_we_sentences, function(x) calculate.similarity_(x)))
  
  return(local_pca_cos_sim_we_sentences)
}

sim.svdpca.embeddings.matrix <- function(sentences, word_vectors = WordVectors, sim.method = "cosine", checkDistance = FALSE, regularization = "none"){
  require(stats)
  require(rsvd)
  
  local_pca_cos_sim_we_sentences <- 1:nrow(sentences)

  calculate.similarity_ <- function(index){
    # Error line 2474
    x <- as.embeddings.matrix(sentences_train[index,1], word_vectors)
    y <- as.embeddings.matrix(sentences_train[index,2], word_vectors)
    
    if(all(x == 0)){
      x <- diag(nrow = nrow(x), ncol = ncol(x))
    }
    
    if(all(y == 0)){
      y <- diag(nrow = nrow(y), ncol = ncol(y))
    }
    
    x <- rpca(x, retx = TRUE)
    y <- rpca(y, retx = TRUE)
    
    x <- as.matrix(x$x) #Utilizar apenas o Score, a PCA 1
    y <- as.matrix(y$x) #Utilizar apenas o Score, a PCA 1
    
    
    if(checkDistance){
      return(dist2(t(x[,1]), t(y[,1]), method = sim.method, norm = regularization))
    }else{
      return(sim2(t(x[,1]), t(y[,1]), method = sim.method, norm = regularization))
    }
  }
  
  local_pca_cos_sim_we_sentences <- unlist(lapply(local_pca_cos_sim_we_sentences, function(x) calculate.similarity_(x)))
  
  return(local_pca_cos_sim_we_sentences)
}


sim.rwmd.embeddings.matrix <- function(sentences, word_vectors = WordVectors, rwmd_model, checkDistance = FALSE, regularization = "none"){
  require(stats)
  
  local_rwmd_cos_sim_we_sentences <- 1:nrow(sentences)
  
  calculate.similarity_ <- function(index){
    
    x <- as.embeddings.matrix(sentences[index,1], word_vectors)
    y <- as.embeddings.matrix(sentences[index,2], word_vectors)

    if(checkDistance){
      return(dist2(t(x[,1]), t(y[,1]), method = rwmd_model, norm = regularization))
    }else{
      return(pdist2 (x, y, method = rwmd_model, norm = regularization))
    }
   
  }
  
  local_rwmd_cos_sim_we_sentences <- unlist(lapply(local_rwmd_cos_sim_we_sentences, function(x) calculate.similarity_(x)))
  
  return(local_rwmd_cos_sim_we_sentences)
}

sim.tsne.embeddings.matrix <- function(sentences, word_vectors = WordVectors, 
                                       sim.method = "cosine", regularization = "none",
                                       mode.perplexity = 50, max.iterations = 1000){
  require(Rtsne)
  
  local_tsne_cos_sim_we_sentences <-c()
  
  for(index in 1:nrow(sentences)){
    
    x <- as.embeddings.matrix(sentences[index,1], word_vectors)
    y <- as.embeddings.matrix(sentences[index,2], word_vectors)
    
    x <- Rtsne(x, dims = 1, perplexity=mode.perplexity, verbose=FALSE, max_iter = max.iterations, check_duplicates = FALSE)
    y <- Rtsne(y, dims = 1, perplexity=mode.perplexity, verbose=FALSE, max_iter = max.iterations, check_duplicates = FALSE)
    
    x <- x$Y #Utilizar apenas o Score, a PCA 1
    y <- y$Y #Utilizar apenas o Score, a PCA 1
    
    local_tsne_cos_sim_we_sentences <- rbind(local_tsne_cos_sim_we_sentences, sim2(t(x), t(y), method = sim.method, norm = regularization))
  }
  
  return(local_tsne_cos_sim_we_sentences)
}

sim.pca.embeddings.vector <- function(sentences, word_vectors = WordVectors, sim.method = "cosine", regularization = "none"){
  require(stats)
  
  local_pca_cos_sim_we_sentences <-c()
  
  for(index in 1:nrow(sentences)){
    
    x <- as.embeddings.vector(sentences[index,1], word_vectors)
    y <- as.embeddings.vector(sentences[index,2], word_vectors)
    
    x <- princomp(x, cor = FALSE)
    y <- princomp(y, cor = FALSE)
    
    x <- as.matrix(x$scores) #Utilizar apenas o Score, a PCA 1
    y <- as.matrix(y$scores) #Utilizar apenas o Score, a PCA 1
    
    local_pca_cos_sim_we_sentences <- rbind(local_pca_cos_sim_we_sentences, sim2(t(x[,1]), t(y[,1]), method = sim.method, norm = regularization))
  }
  
  return(local_pca_cos_sim_we_sentences)
}

sim.tfidf <- function(sentences, sim.method = "cosine", regularization = "none") {
  
  sentences <- as.tfidf(sentences)
  
  local_cos_sim_tfidf_sentences <- c()
  
  for(index in 1:nrow(sentences)){
    t_row <- sentences[index, 1]
    h_row <- sentences[index, 2]
    
    t_row <- strsplit(as.character(t_row), ";")
    t_row <- as.numeric(unlist(t_row))
    t_row <- as.matrix(t_row)
    
    h_row <- strsplit(as.character(h_row), ";")
    h_row <- as.numeric(unlist(h_row))
    h_row <- as.matrix(h_row)
    
    local_cos_sim_tfidf_sentences <- rbind(local_cos_sim_tfidf_sentences, sim2(t(t_row), t(h_row), method = sim.method, norm = regularization))
  }
  
  return(local_cos_sim_tfidf_sentences)
}

sim.diff.penalization <- function(x, y, avereagedSimilarity){
  x <- strsplit(x, "")
  y <- strsplit(y, "")
  
  x <- unlist(x)
  y <- unlist(y)
  
  x <- length(x)
  y <- length(y)
  
  if(x > y){
    penalization <- ((abs(x - y)) * avereagedSimilarity)/x
  }else{
    penalization <- ((abs(x - y)) * avereagedSimilarity)/y
  }
}

sim.hyponym <- function(s1, s2, hyponym_relations, sim.method = "cosine", checkDistance = FALSE, regularization = "none"){
  require(text2vec)

  search.in <- function(x, tokens){
    return(unlist(
      lapply(
        tokens,
        function(y) nrow(hyponym_relations[
          hyponym_relations$target == x &
            hyponym_relations$description == "has_hyponym" &
            hyponym_relations$source == y
          ,])
      )
    )
    )
  }
  
  s1 <- unlist(strsplit(s1, " "))
  s2 <- unlist(strsplit(s2, " "))
  
  s1_onehot <- unlist(lapply(s1, function(x) search.in(x, s2)))
  s2_onehot <- unlist(lapply(s2, function(x) search.in(x, s1)))
  
  s1_onehot <- as.numeric(s1_onehot)
  s2_onehot <- as.numeric(s2_onehot)
  
  if(checkDistance){
    return(dist2(t(s1_onehot), t(s2_onehot), method = sim.method, norm = regularization))
  }else{
    return(sim2(t(s1_onehot), t(s2_onehot), method = sim.method, norm = regularization))
  }
}

sim.hyperonym <- function(s1, s2, hyponym_relations, sim.method = "cosine", checkDistance = FALSE, regularization = "none"){
  require(text2vec)

  search.in <- function(x, tokens){
    return(unlist(
      lapply(
        tokens,
        function(y) nrow(hyponym_relations[
          hyponym_relations$target == x &
            hyponym_relations$description == "has_hyperonym" &
            hyponym_relations$source == y
          ,])
      )
    )
    )
  }
  
  s1 <- unlist(strsplit(s1, " "))
  s2 <- unlist(strsplit(s2, " "))
  
  s1_onehot <- unlist(lapply(s1, function(x) search.in(x, s2)))
  s2_onehot <- unlist(lapply(s2, function(x) search.in(x, s1)))
  
  s1_onehot <- as.numeric(s1_onehot)
  s2_onehot <- as.numeric(s2_onehot)
  
  if(checkDistance){
    return(dist2(t(s1_onehot), t(s2_onehot), method = sim.method, norm = regularization))
  }else{
    return(sim2(t(s1_onehot), t(s2_onehot), method = sim.method, norm = regularization))
  }
}

sim.hyperonym_hyponym <- function(s1, s2, hyponym_relations, sim.method = "cosine", checkDistance = FALSE, regularization = "none"){
  require(text2vec)

  search.in <- function(x, tokens){
    return(unlist(
      lapply(
        tokens,
        function(y) nrow(hyponym_relations[
          hyponym_relations$target == x &
            hyponym_relations$source == y
          ,])
      )
    )
    )
  }
  
  s1 <- unlist(strsplit(s1, " "))
  s2 <- unlist(strsplit(s2, " "))
  
  s1_onehot <- unlist(lapply(s1, function(x) search.in(x, s2)))
  s2_onehot <- unlist(lapply(s2, function(x) search.in(x, s1)))
  
  s1_onehot <- as.numeric(s1_onehot)
  s2_onehot <- as.numeric(s2_onehot)
  
  if(checkDistance){
    return(dist2(t(s1_onehot), t(s2_onehot), method = sim.method, norm = regularization))
  }else{
    return(sim2(t(s1_onehot), t(s2_onehot), method = sim.method, norm = regularization))
  }
}
