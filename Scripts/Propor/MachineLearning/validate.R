
validate.result <- function(predicted, expected_values){
  require(fscaret)

  correlation <- cor.test(expected_values, predicted, method = "pearson")
  error <- MSE(predicted, expected_values, length(predicted))
  
  return(c(cor = correlation$estimate, mse = error))
}