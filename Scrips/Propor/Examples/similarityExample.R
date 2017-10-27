



# http://text2vec.org/similarity.html
# http://stackoverflow.com/questions/39514941/preparing-word-embeddings-in-text2vec-r-package
# https://cran.r-project.org/web/packages/text2vec/vignettes/glove.html
# http://dsnotes.com/articles/glove-enwiki

CustomizeVocab <- function(gloveVocabulary) {
  gloveVocabulary <- as.data.frame(gloveVocabulary)
  gloveVocabulary <- cbind(gloveVocabulary, POSTag(gloveVocabulary[,1])$GetTags())
  
  gloveVocabulary <- as.data.frame(gloveVocabulary)
  
  removable_tags <- c("prp", "art", "conj-c", "pron-indp", "pron-pers", "pron-det", "v-pcp", "conj-s")
  
  gloveVocabulary <- gloveVocabulary[!(gloveVocabulary[,4] %in% removable_tags),]
  return(gloveVocabulary)
}

Glove <- function() {
  require(stringr)
  require(text2vec)
  require(tm)
  
  if (!exists("method_token") || !exists("method_vocab")) {
    print("method_token or method_vocab not exists. Let's creating they")
    
    # Create vocabulary. Terms will be unigrams (simple words).
    method_token <<- itoken(GetGloveTrainSet() %>%  word_tokenizer)
    method_vocab <<- create_vocabulary(method_token, ngram = c(1L, 1L))
    method_vocab <<- prune_vocabulary(method_vocab, term_count_min = 2L, doc_proportion_max = 5L)
  
    method_vectorizer <<- c()
    method_glove_model <- c()
  }
  
  initialize <- function(pairPhrase, vocab_term_count_min = 2L, vectorizer_skip_grams_window = 5L) {
      
      pairPhrase <- create_vocabulary(itoken(pairPhrase  %>% tolower %>% removePunctuation %>% removeNumbers %>%  word_tokenizer))
      pairPhrase <- prune_vocabulary(pairPhrase, term_count_min = vocab_term_count_min)
      
      # Use our filtered vocabulary
      method_vectorizer <<- vocab_vectorizer(pairPhrase,
                                            # don't vectorize input
                                            grow_dtm = FALSE,
                                            # use window of 5 for context words
                                            skip_grams_window = vectorizer_skip_grams_window)
    }
  
  train <- function(glove_word_vectors_size = 50L, glove_x_max = 10) {
      tcm_train <- create_tcm(method_token, method_vectorizer)
      
      method_glove_model <<- GlobalVectors$new(
        word_vectors_size = glove_word_vectors_size,
        vocabulary = method_vocab,
        x_max = glove_x_max
      )
    }
  
  fit_token <- function(tcm_matrix, train_epoch = 10) {
    if (is.null(method_glove_model))
      stop("glove model is null, try to train it before fit")
    
    return(fit(tcm_matrix, method_glove_model, n_iter = train_epoch))
  }
  
  method_create_tcm <-
  function(phrase, vectorizer = method_vectorizer) {
    # Calcula a matriz de distancias de B-T
    token_phrase = itoken(phrase %>% tolower %>% removePunctuation %>% removeNumbers,
                          progressbar = FALSE)
    tcm_phrase = create_tcm(token_phrase, vectorizer)
    
    return(tcm_phrase)
  }
  
  get_word_vectors <- function(){
    return(method_glove_model$get_word_vectors())
  }
  
  list(initialize = initialize,
       train = train,
       fit_token = fit_token,
       create_tcm = method_create_tcm,
       get_word_vectors = get_word_vectors)
}

GetEmbeddingFromGivenSet <- function(word_vectors_model, word) {
  result <- word_vectors_model[word]
  
  if (is.na(result))
    return(as.matrix(t(vector(
      "numeric", ncol(word_vectors_model)
    ))))
  else
    
    return(as.matrix(t(word_vectors_model[word,])))
}

CreateTfIdfMatrix <- function(pairPhrase, vocab_term_count_min = 1L) {
    require(text2vec)
    
    token_phrase = itoken(pairPhrase  %>% tolower  %>% removePunctuation %>% removeNumbers %>% word_tokenizer)
    
    vocab_phrase <- create_vocabulary(token_phrase)
    
    # Use our filtered vocabulary
    vect_phrase <- vocab_vectorizer(vocab_phrase)
    
    tcm_phrase <- create_dtm(token_phrase, vect_phrase)
    
    tfidf_phrase = TfIdf$new() # Cria nova instancia do tfidf
    
    #Calcula a matriz de distancia com tfidf
    tfidf_phrase = fit_transform(tcm_phrase, tfidf_phrase)
    
    return(tfidf_phrase)
  }

b <- {
  require(tm)
  require(text2vec)

  b <- LoadProporData()[, 1:2] # Usa só um registro do propor de teste
  b <- as.matrix(b)
  b <- ReplaceAccentedChraracters(b)
  b <- removePunctuation(b)
  b <- removeNumbers(b)
  b <- tolower(b)
  b <- gsub('\\b\\w{1}\\s','',b) # Remove words with less than two characters
}

pair_sentence <- b[1, 1:2]


pair_sentence_embeddings <- {
  glove <- Glove()
  glove$initialize(b)
  glove$train()
  
  tcm <- glove$create_tcm(phrase = pair_sentence)
  
  tcm_model <- glove$fit_token(tcm)
  
  word_vectors_model <- glove$get_word_vectors()
  
  names(word_vectors_model) <- rownames(word_vectors_model)
  
  splited_phrase_t <- unlist(strsplit(as.character(pair_sentence[1]), " "))
  splited_phrase_h <- unlist(strsplit(as.character(pair_sentence[2]), " "))
  
  splited_phrase_t <- splited_phrase_t[nchar(splited_phrase_t) > 1]
  splited_phrase_h <- splited_phrase_h[nchar(splited_phrase_h) > 1]
  
  phrase_t <- mapply(
    splited_phrase_t,
    FUN = function(x)
      GetEmbeddingFromGivenSet(word_vectors_model, x)
  )
  
  names(phrase_t) <- colnames(phrase_t)
  phrase_t <- t(phrase_t)
  
  phrase_h <- mapply(
    splited_phrase_h,
    FUN = function(x)
      GetEmbeddingFromGivenSet(word_vectors_model, x)
  )
  
  names(phrase_h) <- colnames(phrase_h)
  phrase_h <- t(phrase_h)
  
  pair_sentence_embeddings <- sim2(phrase_t, phrase_h, method = "cosine")
}

pair_sentence_tfidf <- as.matrix(CreateTfIdfMatrix(as.matrix(pair_sentence)))

plot(t(pair_sentence_embeddings), col="red", bg = "red", pch=16)

teste <- lm(formula = t(phrase_t) ~ t(phrase_h), data = as.data.frame(pair_sentence_embeddings))
sum(rowSums(teste$coefficients), na.rm = T)