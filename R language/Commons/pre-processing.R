
Stemming <- function(phrase, currentLanguage = "portuguese"){
  require(SnowballC)
  
  phrase <- as.character(phrase)
  phrase <- strsplit(phrase, " ")
  phrase <- wordStem(unlist(phrase), language = currentLanguage)
  phrase <- paste(phrase, collapse = " ")
  
  return(phrase)
}

POSTag <- function(x, currentLanguage = "pt"){
  require(openNLP)
  require(openNLPmodels.pt)
  require(NLP)
  
  x <- NLP::as.String(x)
  wordAnnotation <- NLP::annotate(
    x,
    list(
      openNLP::Maxent_Sent_Token_Annotator(language = currentLanguage),
      openNLP::Maxent_Word_Token_Annotator(language = currentLanguage)
    )
  )
  
  POSAnnotation <- NLP::annotate(
    x,
    openNLP::Maxent_POS_Tag_Annotator(language = currentLanguage),
    wordAnnotation
  )
  
  POSwords <- subset(POSAnnotation, type == "word")
  tags <- sapply(POSwords$features, '[[', "POS")
  # tokenizedAndTagged <- data.frame(Tokens = x[POSwords], Tags = tags)
  
  return(tags)
}

count.pos <- function(tags, pattern = "(^n$)"){
  tags_matched <- grepl(pattern, tags)
  return(length(tags_matched[tags_matched == TRUE]))
}

ngram <- function(corpus, grams = c(2, 3), minOccurrences = 0){
  
  gramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = grams[1], max = grams[2]))
  tdm <- tm::TermDocumentMatrix(tm::Corpus(tm::VectorSource(corpus)), control = list(tokenize = gramTokenizer))
  tdm <- data.frame(ngram = tdm$dimnames$Terms, frequency = tdm$v)
  
  if(minOccurrences > 0){
    tdm <- tdm[tdm$frequency >= minOccurrences,]
  }
  
  return(tdm)
}

replace.ngram <- function(x, ngrams){

  for(i in 1:nrow(ngrams)){
    x <-   gsub(
      as.character(ngrams$ngram[i]),
      gsub(" ", "_", ngrams$ngram[i]),
      x
    )
  }
  return(x)
}

TermDocument <- function(phrase){
  require(SnowballC)
  require(tm)
  
  if(!(class(phrase)[1] == "VCorpus")) # transforma em documento caso nao seja
    corpus <- Corpus(VectorSource(phrase))
  
  GetSource <- function(){
    return(corpus)
  }
  
  Tf <- function(stopwords = FALSE, stemming = FALSE){
    tempCorpus <- corpus
    
    if(stopwords == TRUE){
      tempCorpus <- tm_map(tempCorpus, removeWords, as.vector(DefaultStopWords$V1))
    }
    if(stemming == TRUE){
      tempCorpus <- tm_map(tempCorpus, stemDocument, getStemLanguages()[11])
    }
    
    return(TermDocumentMatrix(tempCorpus, control = list(weighting = weightTf)))
  }
  
  TfIdf <- function(stopwords = FALSE, stemming = FALSE){
    tempCorpus <- corpus
    
    if(stopwords == TRUE){
      tempCorpus <- tm_map(tempCorpus, removeWords, stopwords("english"))
    }
    if(stemming == TRUE){
      tempCorpus <- tm_map(tempCorpus, stemDocument, getStemLanguages()[11])
    }
    
    result <- as.matrix(TermDocumentMatrix(tempCorpus, control = list(weighting = weightTfIdf)))[,2]
    
    return(result)
  }
  
  list(Get = GetSource, Tf = Tf, TfIdf = TfIdf)
}

CorpusPairSentence <- function(proporData){
  require(tm)
  require(lsa)
  
  GetPairSentence <- function(row){
    sentence1 <- row[1]
    sentence2 <- row[2]
    
    sentence1 <- removePunctuation(sentence1)
    sentence2 <- removePunctuation(sentence2)
    
    return(list(sentence1, sentence2))
  } 
  
  pairSentences <- apply(propor, 1, function(x) GetPairSentence(x))
  
  corpus <- Corpus(VectorSource(pairSentences))
  
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, PlainTextDocument)
  
  GetCorpus <- function(){
    return(corpus)
  }
  
  GetTermDocumentMatrix <- function(){
    td.mat <- as.matrix(TermDocumentMatrix(corpus))
    return(td.mat)
  }
  
  GetTextMatrix <- function(){
    td.mat <- as.matrix(TermDocumentMatrix(corpus))
    
    td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat) # weighting
    lsaSpace <- lsa(td.mat.lsa) # create LSA space
    dist.mat.lsa <- as.textmatrix(lsaSpace) # compute distance matrix
    result <- cosine(dist.mat.lsa[,1], dist.mat.lsa[,2]) # Get euclidean distance result of both sentence similarity
    
    # Like crossprod(x, y)/sqrt(crossprod(x) * crossprod(y))
    
    return(result)
  }
}

term.document.pos <- function(sentence, list_of_tags){
  sentence <- POSTag(unlist(strsplit(sentence, " ")))
  return(unlist(lapply(list_of_tags, function(x) ifelse(x %in% sentence, 1, 0))))
}

replace.sentence.synonym <- function(s1, s2, tep_filepath = "Data/Corpus/pt/tep.json", delaf_filepath = "Data/Corpus/pt/delaf.json"){
  require(jsonlite)
  
  tep_data <- fromJSON(tep_filepath, simplifyDataFrame = TRUE)
  delaf_data <- fromJSON(delaf_filepath, simplifyDataFrame = TRUE)
  
  compare.words <- function(token, sent_tokens){
    if(token %in% sent_tokens){
      return(token)
    }else{
      token_synonym <- unlist(tep_data[delaf_data[token]])
      if(is.null(token_synonym)){
        return(token)
      }else{
        token_synonym <- unlist(lapply(token_synonym, function(x) ifelse(x %in% sent_tokens, x, NA)))
        
        token_synonym <- token_synonym[!is.na(token_synonym)]
        
        if(length(token_synonym) == 0){
          return(token)
        }else{
          return(token_synonym[1])
        }
      }
    }
  }
  
  s1 <- unlist(strsplit(s1, " "))
  s2 <- unlist(strsplit(s2, " "))
  
  s1 <- unlist(lapply(s1, function(x) compare.words(x, s2)))
  
  return(paste(s1, collapse = " "))
}

replace.to.delaf <- function(sentence, delaf_data){
  sentence <- unlist(strsplit(sentence, " "))
  
  process <- function(x){
    delafx <- delaf_data[x]
    delafx <- unlist(delafx)
    
    if(is.null(delafx)){
      return(x)
    }else{
      return(delafx)
    }
  }
  
  sentence <- unlist(lapply(sentence, function(x) ifelse(is.null(unlist(delaf_data)))))
  
  return(paste(sentence, collapse = " "))
}
