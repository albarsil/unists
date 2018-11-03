

glove.train <- function(corpus = NULL, ngram_bounduary = c(1L, 1L), token_min = 3L, skip_gram_window = 6L,
                        dimension_window = 600L, x_max_value = 100L, learning_rate_value = 0.15, 
                        epochs = 10L, convergence_threshold = 0.005, num_threads = 4L, shuffle_value = 42L){
  require(text2vec)
  
  if(is.null(corpus)){
    local_token <- read.glove.corpus() %>%  space_tokenizer
  }
  else{
    local_token <- corpus %>%  space_tokenizer
  }
  
  local_itoken <- itoken(local_token)
  local_vocab <- create_vocabulary(local_itoken, ngram = ngram_bounduary)
  
  print(paste(">>> Vocabulário bruto: ", nrow(local_vocab$vocab)))
  
  local_vocab <- prune_vocabulary(vocabulary = local_vocab, 
                                  term_count_min = token_min) # Filter terms that appear more than 30% in the documents
  
  print(paste(">>> Vocabulário final: ", nrow(local_vocab$vocab)))
  
  # Use our filtered vocabulary
  local_vectorizer <- vocab_vectorizer(local_vocab,
                                       # don't vectorize input
                                       grow_dtm = FALSE,
                                       # use window of 5 for context words
                                       skip_grams_window = skip_gram_window)
  local_tcm <- create_tcm(local_itoken, local_vectorizer)
  
  # explicitly set number of threads
  RcppParallel::setThreadOptions(numThreads = num_threads)
  
  local_glove_model <- GlobalVectors$new(vocabulary = local_vocab, 
                                         word_vectors_size = dimension_window, 
                                         learning_rate = learning_rate_value,
                                         x_max = x_max_value, 
                                         shuffle = shuffle_value)
  
  local_glove_model <- fit(local_tcm, local_glove_model, n_iter = epochs, convergence_tol = convergence_threshold) # we will stop if global cost will be reduced less then 0.5% then previous SGD iteration
  
  return(local_glove_model)
}

glove.king_test <- function(word_vectors, language = "pt", sim.method = "cosine", regularization = "none"){
  require(text2vec)
  
  if(language == "pt"){
    words <- c("rei", "homem", "mulher")
  }
  else{
    words <- c("king", "men", "woman")
  }
  
  
  if(!is.matrix(word_vectors))
    word_vectors <- as.matrix(word_vectors)
  
  #word_vectors["rei", , drop = FALSE] - word_vectors["homem", , drop = FALSE] + word_vectors["mulher", , drop = FALSE]
  
  rainha <- word_vectors[words[1], , drop = FALSE] - 
    word_vectors[words[2], , drop = FALSE] + 
    word_vectors[words[3], , drop = FALSE]
  
  cos_sim = sim2(x = word_vectors, y = rainha, method = sim.method, norm = regularization)
  head(sort(cos_sim[,1], decreasing = TRUE), 5L)
}

read.glove.corpus <- function(filepath) {
  require(data.table)
  require(text2vec)
  require(tm)
  
  progressValue <- 0
  progressBar <- txtProgressBar(min = 0, max = 100, initial = 0, char = "=",
                                width = NA, "Reading GloVe corpus", "0%", style = 3, file = "")
  
  updateProgressBar <- function(){
    
    # Progress will be regarded to calls in the function
    # 13 instructions = 7,69 per call
    
    progressValue <<- progressValue + 7.69
    setTxtProgressBar(progressBar, progressValue, title = "Reading GloVe corpus", label = paste(progressValue, "%")) # Increment progress
  }
  
  gloveTable <- read.table(
    filepath,
    header = F,
    sep = "\n",
    stringsAsFactors = F,
    # data.table = FALSE,
    encoding = "UTF-8"
  )
  
  progress.time <- Sys.time()
  
  gloveTable <- as.vector(gloveTable)
  updateProgressBar()
  
  gloveTable <- gsub("([[:punct:]])", " \\1 ", as.character(gloveTable))
  updateProgressBar()
  
  gloveTable <- gsub("\\s\\s+", " ", as.character(gloveTable))
  updateProgressBar()
  
  gloveTable <- as.character(gloveTable)
  updateProgressBar()
  
  gloveTable <- tolower(gloveTable)
  updateProgressBar()
  close(progressBar)
  
  print(paste("Elapsed time: ", Sys.time() - progress.time))
  
  return(gloveTable)
}
