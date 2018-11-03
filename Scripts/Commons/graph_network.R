
replace.sentence.hyponym <- function(graph_net, s1, s2){
  
  pulo.data <- graph_net$get.pulo_data()
  
  compare_hypo_and_hyper <- function(token, sent_token){
    hyponym_tokens <- pulo.data[which(pulo.data$source == token & pulo.data$description == "has_hyperonym"),]
    hyperonym_tokens <- pulo.data[which(pulo.data$source == token & pulo.data$description == "has_hyponym"),]
    
    if(token == sent_token){
      return(token)
    }
    else if(sent_token %in% hyperonym_tokens$target){
      return(sent_token)
    }else if(sent_token %in% hyponym_tokens$target){
      return(token)
    }else{
      return(NA)
    }
  }
  
  check_hypo_and_hyper <- function(token, sentence_tokens){
    sent <- unlist(lapply(sentence_tokens, function(x) compare_hypo_and_hyper(token, x)))
    sent <- sent[!is.na(sent)]
    
    if(length(sent) > 0){
      return(sent[1])
    }
    else{
      return(token)
    }
  }
  
  s1 <- unlist(strsplit(s1, " "))
  s2 <- unlist(strsplit(s2, " "))
  
  s <- unlist(lapply(s1, function(x) check_hypo_and_hyper(x, s2)))
  
  return(paste(s, collapse = " "))
}

count.antonym <- function(antonym_distances, sentences){
  
  count_antonym_sentences <- function(s1, s2){
    s <- sentence_distances(antonym_distances, s1, s2, TRUE)
    return(length(s[s == 1]))
  }
  
  return(apply(sentences, 1, function(x) count_antonym_sentences(x[1], x[2])))
}

sentence_distances <- function(graph_distances, s1, s2, na.as.zero = FALSE){
  s1 <- unlist(strsplit(s1, " "))
  s2 <- unlist(strsplit(s2, " "))
  
  m <- sapply(s1, function(x) vapply(s2, function(y) path.length(graph_distances, x, y), FUN.VALUE = numeric(1L)))
  
  if(na.as.zero){
    m[is.na(m)] <- 0
  }
  
  return(m)
}

expand_sentences <- function(synonym_net, s1, s2){
  s1 <- unlist(strsplit(s1, " "))
  s2 <- unlist(strsplit(s2, " "))
  
  synonym <- get.nearest(synonym_net)
  
  find.synonym <- function(word, s_tokens){
    if(word %in% s_tokens){
      return(word)
    }else{
      syn <- synonym$get(word)
      
      if(length(syn[!is.na(syn)]) == 0){
        return(word)
      }else{
        syn <- unlist(lapply(syn, function(x) ifelse(x %in% s_tokens, x, NA)))
        
        syn <- syn[!is.na(syn)]
        
        if(length(syn) == 0){
          return(word)
        }else{
          return(syn[1])
        }
      }
    }
  }
  
  s1 <- unlist(lapply(s1, function(x) find.synonym(x, s2)))
  s1 <- paste(s1, collapse = " ")
  
  return(s1)
}

path.length <- function(graph_distances, word1, word2){
  if(word1 %in% rownames(graph_distances) && word2 %in% rownames(graph_distances)){
    dist.w <- graph_distances[word1,word2]
    return(ifelse(is.finite(dist.w), dist.w, NA))
  }else{
    return(NA)
  }
}

pulo.network <- function(filepath = "Data/relations.csv"){
  require(data.table)
  require(igraph)
  
  pulo.data <- fread(filepath, sep = ",", header = TRUE, encoding = "UTF-8", data.table = FALSE)
  pulo.data$description <- as.factor(pulo.data$description)
  
  get.pulo_data <- function(){
    return(pulo.data)
  }
  
  hyponym <- function(will = "all"){
    
    if(will == "hyponym"){
      hyponymy_matrix <- pulo.data[pulo.data$description == 'has_hyponym',]
    }else if(will == "hyperonym"){
      hyponymy_matrix <- pulo.data[pulo.data$description == 'has_hyperonym',]
    }else{
      hyponymy_matrix <- pulo.data[pulo.data$description == 'has_hyponym' | pulo.data$description == 'has_hyperonym',]
    }
    
    net <- graph(c(t(hyponymy_matrix[,-2])), directed = TRUE)
    net <- simplify(net, remove.multiple = F, remove.loops = T) 
    
    return(net)
  }
  
  near_antonym <- function(){
    hyponymy_matrix <- pulo.data[pulo.data$description == 'near_antonym',]
    
    net <- graph(c(t(hyponymy_matrix[,-2])), directed = TRUE)
    net <- simplify(net, remove.multiple = F, remove.loops = T) 
    
    return(net)
  }
  
  near_synonym <- function(){
    hyponymy_matrix <- pulo.data[pulo.data$description == 'near_synonym',]
    
    net <- graph(c(t(hyponymy_matrix[,-2])), directed = TRUE)
    net <- simplify(net, remove.multiple = F, remove.loops = T) 
    
    return(net)
  }
  
  related_to <- function(){
    hyponymy_matrix <- pulo.data[pulo.data$description == 'related_to',]
    
    net <- graph(c(t(hyponymy_matrix[,-2])), directed = TRUE)
    net <- simplify(net, remove.multiple = F, remove.loops = T) 
    
    return(net)
  }
  
  causes <- function(){
    hyponymy_matrix <- pulo.data[pulo.data$description == 'causes',]
    
    net <- graph(c(t(hyponymy_matrix[,-2])), directed = TRUE)
    net <- simplify(net, remove.multiple = F, remove.loops = T) 
    
    return(net)
  }
  
  category_term <- function(){
    hyponymy_matrix <- pulo.data[pulo.data$description == 'category_term',]
    
    net <- graph(c(t(hyponymy_matrix[,-2])), directed = TRUE)
    net <- simplify(net, remove.multiple = F, remove.loops = T) 
    
    return(net)
  }
  
  be_in_state <- function(){
    hyponymy_matrix <- pulo.data[pulo.data$description == 'be_in_state',]
    
    net <- graph(c(t(hyponymy_matrix[,-2])), directed = TRUE)
    net <- simplify(net, remove.multiple = F, remove.loops = T) 
    
    return(net)
  }
  
  has_derived <- function(){
    hyponymy_matrix <- pulo.data[pulo.data$description == 'has_derived',]
    
    net <- graph(c(t(hyponymy_matrix[,-2])), directed = TRUE)
    net <- simplify(net, remove.multiple = F, remove.loops = T) 
    
    return(net)
  }
  
  has_holo_madeof <- function(){
    hyponymy_matrix <- pulo.data[pulo.data$description == 'has_holo_madeof',]
    
    net <- graph(c(t(hyponymy_matrix[,-2])), directed = TRUE)
    net <- simplify(net, remove.multiple = F, remove.loops = T) 
    
    return(net)
  }
  
  has_holo_member <- function(){
    hyponymy_matrix <- pulo.data[pulo.data$description == 'has_holo_member',]
    
    net <- graph(c(t(hyponymy_matrix[,-2])), directed = TRUE)
    net <- simplify(net, remove.multiple = F, remove.loops = T) 
    
    return(net)
  }
  
  has_holo_part <- function(){
    hyponymy_matrix <- pulo.data[pulo.data$description == 'causes',]
    
    net <- graph(c(t(hyponymy_matrix[,-2])), directed = TRUE)
    net <- simplify(net, remove.multiple = F, remove.loops = T) 
    
    return(net)
  }
  
  has_subevent <- function(){
    hyponymy_matrix <- pulo.data[pulo.data$description == 'has_subevent',]
    
    net <- graph(c(t(hyponymy_matrix[,-2])), directed = TRUE)
    net <- simplify(net, remove.multiple = F, remove.loops = T) 
    
    return(net)
  }
  
  rgloss <- function(){
    hyponymy_matrix <- pulo.data[pulo.data$description == 'rgloss',]
    
    net <- graph(c(t(hyponymy_matrix[,-2])), directed = TRUE)
    net <- simplify(net, remove.multiple = F, remove.loops = T) 
    
    return(net)
  }
  
  region_term <- function(){
    hyponymy_matrix <- pulo.data[pulo.data$description == 'region_term',]
    
    net <- graph(c(t(hyponymy_matrix[,-2])), directed = TRUE)
    net <- simplify(net, remove.multiple = F, remove.loops = T) 
    
    return(net)
  }
  
  usage_term <- function(){
    hyponymy_matrix <- pulo.data[pulo.data$description == 'usage_term',]
    
    net <- graph(c(t(hyponymy_matrix[,-2])), directed = TRUE)
    net <- simplify(net, remove.multiple = F, remove.loops = T) 
    
    return(net)
  }
  
  list(get.pulo_data = get.pulo_data, hyponym = hyponym, near_antonym = near_antonym, near_synonym = near_synonym, related_to = related_to, causes = causes, category_term = category_term, be_in_state = be_in_state, has_derived = has_derived, has_holo_madeof = has_holo_madeof, has_holo_member = has_holo_member, has_holo_part = has_holo_part, has_subevent = has_holo_part, rgloss = rgloss, region_term = region_term, usage_term = usage_term)
}

get.nearest <- function(graph_net){
  graph.dist <- distances(graph_net)
  
  get <- function(word){
    if(word %in% rownames(graph.dist)){
      word <- graph.dist[word, ]
      word <- sort(word)
      
      word <- word[word <= 1]
      word <- word[-1]
      
      return(names(word))
    }else{
      return(NA)
    }
  }
  
  list(get = get)
}