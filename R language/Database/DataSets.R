
read.stopwords <- function(stopwords_path = "Data/stopwords-pt.txt") {
  require(data.table)
  
  return(
    read.table(
      stopwords_path,
      sep = "\n",
      header = FALSE,
      stringsAsFactors = FALSE,
      encoding = "UTF-8"
    )
  )
}

read.propor.dataset <- function(filepath, processing = TRUE){
  require(xml2)
  require(tm)
  require(text2vec)
  require(stringi)
  
  doc <- read_xml(filepath)
  
  t = xml_find_all(doc, "//t/text()")
  h = xml_find_all(doc, "//h/text()")
  
  t <- trimws(xml_text(t))
  h <- trimws(xml_text(h))
  
  entailment = xml_find_all(doc, "//@entailment")
  entailment <- trimws(xml_text(entailment))
  
  similarity = xml_find_all(doc, "//@similarity")
  similarity <- as.numeric(trimws(xml_text(similarity)))
  
  # Create data frame
  local_data <- data.frame(t = t, h = h, entailment = entailment, similarity = similarity)
  
  # Transform it into matrix
  local_data <- as.matrix(local_data)
  
  # Convert text encode to ASCII
  #local_data[,1:2] <- stri_trans_general(local_data[,1:2], "Latin-ASCII")
  
  if(processing){
    # Do some pre-processing to remove accented characters, punctuation and numbers
    local_data[,1] <- gsub("([[:punct:]])", " \\1 ", as.character(local_data[,1]))
    local_data[,1] <- gsub("\\s\\s+", " ", as.character(local_data[,1]))
    
    local_data[,2] <- gsub("([[:punct:]])", " \\1 ", as.character(local_data[,2]))
    local_data[,2] <- gsub("\\s\\s+", " ", as.character(local_data[,2]))
    
    # Convert all characters to low case
    local_data[,1] <- tolower(local_data[,1])
    local_data[,2] <- tolower(local_data[,2])
    
    local_data[,1] <- unlist(lapply(as.character(local_data[,1]), function(x) trimws(x, "both")))
    local_data[,2] <- unlist(lapply(as.character(local_data[,2]), function(x) trimws(x, "both")))
  }
  
  local_data[,4] <- as.numeric(local_data[,4])
  
  return(local_data)
}