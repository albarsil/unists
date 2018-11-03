
count.words <- function(x_list){
  
  word.sentence.counter <- function(elm){
    elm <- strsplit(elm, " ")
    elm <- unlist(elm)
    return(length(elm))
  }
  
  if(is.list(x_list)){
    return(lapply(x_list, FUN = function(x) word.sentence.counter(x)))
  }
  else if(is.character(x_list)){
    return(word.sentence.counter(x_list))
  }
  else{
    stop("The parameter need to be a list or a character")
  }
}


experiment.svm <- function(train.data, test.data, selectedIndex){
  require(e1071)
  
  initialTime <- Sys.time()
  
  target_column <- selectedIndex[length(selectedIndex)]
  progressValue <- 0
  progressBar <- txtProgressBar(min = 0, max = 100, initial = 0, char = "=",
                                width = NA, "Reading GloVe corpus", "0%", style = 3, file = "")
  
  updateProgressBar <- function(){
    
    # Progress will be regarded to calls in the function
    progressValue <<- progressValue + (1/length(exp_combinations))
    setTxtProgressBar(progressBar, progressValue, title = "Doing experiment", label = paste((progressValue * 100), "%")) # Increment progress
  }
  
  get_formula <- function(columns){
    targ <- colnames(train.data)[target_column]
    targ <- paste(targ, "~", collapse = "", sep = "")
    return(paste(targ, paste(colnames(train.data)[columns], collapse = " + ")))
  }
  
  
  get_combinations <- function(x){
    res <- Map(combn, list(x), seq_along(x), simplify = FALSE)
    return(unlist(res, recursive = FALSE))
  }
  
  do_experiment <- function(index_comb){
    updateProgressBar()
    index_comb <- c(index_comb)
    
    svm_model <- svm(formula(get_formula(index_comb)), train.data, cost = 1, cross = 10, gamma = 0.01, kernel = c("radial"), type = "eps-regression")
    
    radial.predict.values <- predict(svm_model, test.data[,-target_column])
    radial.predict <- validate.result(radial.predict.values, test.data[,target_column])
    
    return(radial.predict)
  }
  
  exp_combinations <- get_combinations(selectedIndex)
  exp_combinations <- exp_combinations[-length(selectedIndex)]
  invalid_combinations <- unlist(lapply(exp_combinations, function(x) target_column %in% x))
  
  exp_combinations <- exp_combinations[!invalid_combinations]
  
  exp_results <- sapply(exp_combinations, function(x) do_experiment(unlist(x)))
  exp_results <- t(exp_results)
  exp_results <- as.data.frame(exp_results)
  
  exp_results$description <- unlist(lapply(exp_combinations, function(x) get_formula(unlist(x))))
  
  return(exp_results)
}

experiment.mlp <- function(train.data, test.data, selectedIndex, epochs, hidden_neurons, learning_rate){
  require(nnet)
  
  initialTime <- Sys.time()
  
  target_column <- selectedIndex[length(selectedIndex)]
  progressValue <- 0
  progressBar <- txtProgressBar(min = 0, max = 100, initial = 0, char = "=",
                                width = NA, "Reading GloVe corpus", "0%", style = 3, file = "")
  
  updateProgressBar <- function(){
    
    # Progress will be regarded to calls in the function
    progressValue <<- progressValue + (1/length(exp_combinations))
    setTxtProgressBar(progressBar, progressValue, title = "Doing experiment", label = paste((progressValue * 100), "%")) # Increment progress
  }
  
  get_formula <- function(columns){
    targ <- colnames(train.data)[target_column]
    targ <- paste(targ, "~", collapse = "", sep = "")
    return(paste(targ, paste(colnames(train.data)[columns], collapse = " + ")))
  }
  
  
  get_combinations <- function(x){
    res <- Map(combn, list(x), seq_along(x), simplify = FALSE)
    return(unlist(res, recursive = FALSE))
  }
  
  
  do_experiment <- function(index_comb){
    updateProgressBar()
    
    test_combinations <- index_comb
    index_comb <- c(index_comb)
    
    mlp_model <- nnet(formula(get_formula(index_comb)), train.data, size = hidden_neurons, maxit = epochs, linout = T, decay = learning_rate, trace = FALSE)
    predict.values <- predict(mlp_model, test.data[,-target_column])
    predict.values <- validate.result(predict.values, test.data[,target_column])
    
    return(predict.values)
  }
  
  max = apply(train.data[,-target_column] , 2 , max)
  min = apply(train.data[,-target_column], 2 , min)
  train.data[,-target_column] = as.data.frame(scale(train.data[,-target_column], center = min, scale = max - min))
  
  max = apply(test.data[,-target_column] , 2 , max)
  min = apply(test.data[,-target_column], 2 , min)
  test.data[,-target_column] = as.data.frame(scale(test.data[,-target_column], center = min, scale = max - min))
  
  exp_combinations <- get_combinations(selectedIndex)
  exp_combinations <- exp_combinations[-length(selectedIndex)]
  invalid_combinations <- unlist(lapply(exp_combinations, function(x) target_column %in% x))
  
  exp_combinations <- exp_combinations[!invalid_combinations]
  
  exp_results <- sapply(exp_combinations, function(x) do_experiment(unlist(x)))
  exp_results <- t(exp_results)
  exp_results <- as.data.frame(exp_results)
  
  exp_results$description <- unlist(lapply(exp_combinations, function(x) get_formula(unlist(x))))
  exp_results$epochs <- epochs
  exp_results$hidden_neurons <- paste(hidden_neurons, collapse = "-")
  exp_results$learning_rate <- learning_rate
  
  return(exp_results)
}

experiment.glm <- function(train.data, test.data, selectedIndex){
  require(e1071)
  
  initialTime <- Sys.time()
  
  target_column <- selectedIndex[length(selectedIndex)]
  progressValue <- 0
  progressBar <- txtProgressBar(min = 0, max = 100, initial = 0, char = "=",
                                width = NA, "Reading GloVe corpus", "0%", style = 3, file = "")
  
  updateProgressBar <- function(){
    
    # Progress will be regarded to calls in the function
    progressValue <<- progressValue + (1/length(exp_combinations))
    setTxtProgressBar(progressBar, progressValue, title = "Doing experiment", label = paste((progressValue * 100), "%")) # Increment progress
  }
  
  get_formula <- function(columns){
    targ <- colnames(train.data)[target_column]
    targ <- paste(targ, "~", collapse = "", sep = "")
    return(paste(targ, paste(colnames(train.data)[columns], collapse = " + ")))
  }
  
  
  get_combinations <- function(x){
    res <- Map(combn, list(x), seq_along(x), simplify = FALSE)
    return(unlist(res, recursive = FALSE))
  }
  
  do_experiment <- function(index_comb){
    updateProgressBar()
    index_comb <- c(index_comb)
    
    ml_model <- glm(formula(get_formula(index_comb)), train.data, family = gaussian())
    
    predict.values <- predict(ml_model, test.data[,-target_column])
    predict.values <- validate.result(predict.values, test.data[,target_column])
    
    return(predict.values)
  }
  
  exp_combinations <- get_combinations(selectedIndex)
  exp_combinations <- exp_combinations[-length(selectedIndex)]
  invalid_combinations <- unlist(lapply(exp_combinations, function(x) target_column %in% x))
  
  exp_combinations <- exp_combinations[!invalid_combinations]
  
  exp_results <- sapply(exp_combinations, function(x) do_experiment(unlist(x)))
  exp_results <- t(exp_results)
  exp_results <- as.data.frame(exp_results)
  
  exp_results$description <- unlist(lapply(exp_combinations, function(x) get_formula(unlist(x))))
  
  return(exp_results)
}

experiment.grnn <- function(train.data, test.data, selectedIndex){
  require(nnet)
  
  initialTime <- Sys.time()
  
  target_column <- selectedIndex[length(selectedIndex)]
  progressValue <- 0
  progressBar <- txtProgressBar(min = 0, max = 100, initial = 0, char = "=",
                                width = NA, "Reading GloVe corpus", "0%", style = 3, file = "")
  
  updateProgressBar <- function(){
    
    # Progress will be regarded to calls in the function
    progressValue <<- progressValue + (1/length(exp_combinations))
    setTxtProgressBar(progressBar, progressValue, title = "Doing experiment", label = paste((progressValue * 100), "%")) # Increment progress
  }
  
  get_formula <- function(columns){
    targ <- colnames(train.data)[target_column]
    targ <- paste(targ, "~", collapse = "", sep = "")
    return(paste(targ, paste(colnames(train.data)[columns], collapse = " + ")))
  }
  
  
  get_combinations <- function(x){
    res <- Map(combn, list(x), seq_along(x), simplify = FALSE)
    return(unlist(res, recursive = FALSE))
  }
  
  predict.grnn <- function(nn, x){
    
    if(is.null(dim(x))){
      return(unlist(lapply(x, function(i) guess(nn, i))))
    }
    else{
      xlst <- split(x, 1:nrow(x))
      pred <- foreach(i = xlst, .combine = rbind) %dopar% {
        data.frame(pred = guess(nn, as.matrix(i)), i, row.names = NULL)
      }
      return(pred[,1])
    }
  }
  
  # SEARCH FOR THE OPTIMAL VALUE OF SIGMA BY THE VALIDATION SAMPLE
  grnn_nn <- function(t_data){
    require(grnn)
    require(foreach)
    
    # if data is a vector not a data.frame
    if(is.null(dim(t_data[, -ncol(t_data)]))){
      cv <- foreach(s = seq(0.1, 1, 0.05), .combine = rbind) %dopar% {
        gr <- grnn::smooth(grnn::learn(t_data, variable.column = t_data[, ncol(t_data)]), sigma = s)
        pred <- predict.grnn(gr, t_data[, -ncol(t_data)])
        test.sse <- sum((t_data[, ncol(t_data)] - pred)^2)
        data.frame(s, sse = test.sse)
      }
    }else{
      cv <- foreach(s = seq(0.1, 1, 0.05), .combine = rbind) %dopar% {
        gr <- grnn::smooth(grnn::learn(t_data, variable.column = ncol(t_data)), sigma = s)
        pred <- predict.grnn(gr, t_data[, -ncol(t_data)])
        test.sse <- sum((t_data[, ncol(t_data)] - pred)^2)
        data.frame(s, sse = test.sse)
      }
    }
    
    best.s <- cv[cv$sse == min(cv$sse), 1]
    gr <- grnn::smooth(grnn::learn(t_data, variable.column = ncol(t_data)), sigma = best.s)
    return(gr)
  }
  
  do_experiment <- function(index_comb){
    updateProgressBar()

    print(index_comb)
    grnn_model <- grnn_nn(train.data[, c(index_comb, target_column)])
    predict.values <- predict.grnn(grnn_model, test.data[, c(index_comb)])
    predict.values <- validate.result(predict.values, test.data[,target_column])
    
    return(predict.values)
  }
  
  max = apply(train.data[,-target_column] , 2 , max)
  min = apply(train.data[,-target_column], 2 , min)
  train.data[,-target_column] = as.data.frame(scale(train.data[,-target_column], center = min, scale = max - min))
  
  max = apply(test.data[,-target_column] , 2 , max)
  min = apply(test.data[,-target_column], 2 , min)
  test.data[,-target_column] = as.data.frame(scale(test.data[,-target_column], center = min, scale = max - min))
  
  exp_combinations <- get_combinations(selectedIndex)
  exp_combinations <- exp_combinations[-length(selectedIndex)]
  invalid_combinations <- unlist(lapply(exp_combinations, function(x) target_column %in% x))
  
  exp_combinations <- exp_combinations[!invalid_combinations]
  
  exp_results <- sapply(exp_combinations, function(x) do_experiment(unlist(x)))
  exp_results <- t(exp_results)
  exp_results <- as.data.frame(exp_results)
  
  exp_results$description <- unlist(lapply(exp_combinations, function(x) get_formula(unlist(x))))
  exp_results$hidden_neurons <- paste(hidden_neurons, collapse = "-")
  exp_results$learning_rate <- learning_rate
  
  return(exp_results)
}

