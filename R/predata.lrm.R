#'@title  predata.lrm
#'@description Generate the predicted data for the function. This is needed for drawing.
#'@param fit Model function required for prediction.
#'@param variables variable name.
#'@param y the value of the variable.
#'@return Data required for plotting.
#'
predata.lrm<-function(fit,variables,y){
  assign("fit", fit)
  x<-"x"
  formula <- ""
  formula <- paste(formula,variables,sep="")
  formula <- paste(formula,"=",x,sep="")
  formula <- paste(" Pre0 <- rms::Predict(fit, ",
                   formula,sep="")
  formula <- paste(formula,",fun=exp,ref.zero=T,conf.int = 0.95,digits=2)")
  call1<-call(formula)
  x<-y
  eval(parse(text=call1))
  return(Pre0)
}
