

GloveNetwork <- function(ngram_bounduary = c(1L, 1L), token_min = 2L, skip_gram_window = 5L){
require(text2vec)

local_token <- GetGloveTrainSet() %>%  space_tokenizer
local_itoken <- itoken(local_token)
local_vocab <- create_vocabulary(local_itoken, ngram = ngram_bounduary)
local_vocab <- prune_vocabulary(vocabulary = local_vocab, 
                                 term_count_min = token_min) # Filter terms that appear more than 30% in the documents

  # Use our filtered vocabulary
local_vectorizer <- vocab_vectorizer(local_vocab,
                                         # don't vectorize input
                                         grow_dtm = FALSE,
                                         # use window of 5 for context words
                                         skip_grams_window = skip_gram_window)
local_tcm <- create_tcm(local_itoken, local_vectorizer)
local_glove_model <- c()

get.token <- function(){
  return(local_token)
}

get.itoken <- function(){
  return(local_itoken)
}

get.vocabulary <- function(){
  return(local_vocab)
}

get.tcm <- function(){
  return(local_tcm)
}

train <- function(
  dimension_window = 600L, x_max_value = 100L, learning_rate_value = 0.15, 
  epochs = 10L, convergence_threshold = 0.005, num_threads = 4L, shuffle_value = 42L){

# explicitly set number of threads
RcppParallel::setThreadOptions(numThreads = num_threads)

local_glove_model <<- GlobalVectors$new(vocabulary = local_vocab, 
             word_vectors_size = dimension_window, 
             learning_rate = learning_rate_value,
             x_max = x_max_value, 
             shuffle = shuffle_value)

local_glove_model <<- fit(local_tcm, local_glove_model, n_iter = epochs, convergence_tol = convergence_threshold) # we will stop if global cost will be reduced less then 0.5% then previous SGD iteration

return(local_glove_model)
}

test.king <- function(){
  word_vectors <- local_glove_model$get_word_vectors()
  
  #word_vectors["rei", , drop = FALSE] - word_vectors["homem", , drop = FALSE] + word_vectors["mulher", , drop = FALSE]
  
  rainha <- word_vectors["rei", , drop = FALSE] - 
    word_vectors["homem", , drop = FALSE] + 
    word_vectors["mulher", , drop = FALSE]
  cos_sim = sim2(x = word_vectors, y = rainha, method = "cosine", norm = "l2")
  head(sort(cos_sim[,1], decreasing = TRUE), 5L)
}

list(train = train, test.king = test.king, get.token = get.token, get.vocabulary = get.vocabulary, get.tcm = get.tcm, get.itoken = get.itoken)
}

GetPreparedProporData <- function(){
  require(tm)
  require(text2vec)
  
  local_data <- LoadProporData()[, 1:2] # Usa só um registro do propor de teste
  local_data <- as.matrix(local_data)
  local_data <- ReplaceAccentedChraracters(local_data)
  local_data <- removePunctuation(local_data)
  local_data <- removeNumbers(local_data)
  local_data <- tolower(local_data)
  local_data <- gsub('\\b\\w{1}\\s','',local_data) # Remove words with less than two characters
  
  return(local_data)
}

glove_network <- GloveNetwork()
glove_model <- glove_network$train()
word_vectors <- glove_model$get_word_vectors()
glove_network$test.king()

pair_sentence <- GetPreparedProporData()[1, 1:2]

splited_phrase_t <- unlist(strsplit(as.character(pair_sentence[1]), " "))
splited_phrase_h <- unlist(strsplit(as.character(pair_sentence[2]), " "))

splited_phrase_t <- as.vector(splited_phrase_t[nchar(splited_phrase_t) > 1])
splited_phrase_h <- as.vector(splited_phrase_h[nchar(splited_phrase_h) > 1])

embeddings_phrase_t <- c()
for(el in splited_phrase_t){
  el <- word_vectors[el, , drop = FALSE]
  embeddings_phrase_t <- rbind(embeddings_phrase_t,el)
}

embeddings_phrase_h <- c()
for(el in splited_phrase_h){
  el <- word_vectors[el, , drop = FALSE]
  embeddings_phrase_h <- rbind(embeddings_phrase_h,el)
}

embeddings_phrase_h <- as.matrix(rowSums(t(embeddings_phrase_h)))
embeddings_phrase_t <- as.matrix(rowSums(t(embeddings_phrase_t)))

cos_sim <- sim2(t(embeddings_phrase_t), t(embeddings_phrase_h), method = "cosine", norm = "l2")

#cos_sim <- as.data.frame(cos_sim)
target <- 2.5

model1 <- lm(target ~ ., data = cos_sim)
summary(model1)

mydata <- data.frame(cos_sim=c(0.45601, 0.58, 0.7), target=c(2.5, 3, 1))
model1 <- lm(target ~ cos_sim, data = mydata)
summary(model1)
plot(model1)

