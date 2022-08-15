#'@title  predata
#'@description Generate the predicted data for the function. This is needed for drawing.
#'@param fit Model function required for prediction.
#'@param variables variable name.
#'@param y the value of the variable.
#'@return Data required for plotting.
#'
predata <- function(fit,variables,y){
  UseMethod("predata")
}
