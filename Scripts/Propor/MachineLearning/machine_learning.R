

minimizeMSE <- function (data, lev = NULL, model = NULL) {
  require(fscaret)
  
  out <- MSE(data$pred, data$obs, nrow(data))
  names(out) <- "minimizeMSE"
  out
}

maximizePearson <- function (data, lev = NULL, model = NULL) {
  out <- cor.test(data$pred, data$obs, method = "pearson")
  out <- 1 - out$estimate
  names(out) <- "maximizePearson"
  out
}

minimizeErrorCorrelation <- function (data, lev = NULL, model = NULL) {
  out.cor <- cor.test(data$pred, data$obs, method = "pearson")
  out.mse <- MSE(data$pred, data$obs, nrow(data))
  
  out.cor <- 1 - out.cor$estimate
  
  out <- cbind(out.cor, out.mse)
  out <- apply(out, 1, function(x) x[1] + x[2])
  out <- out[,1]

  names(out) <- "minimizeErrorCorrelation"
  out
}

# Tunning parameters. 10-fold Cross Validation
KfoldCrossValidation <- function(kfold, savePredictions, sum_function = MSEsummaryFunction){
  require(caret)
  
  return(
    trainControl(
      method = "cv",
      number = kfold,
      repeats = kfold,
      savePred = savePredictions,
      summaryFunction = sum_function
    )
  )
}
