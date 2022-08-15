#'@title  ggrcs
#'
#'@description  A function to draw histograms and restricted cubic splines (RCS).
#'
#'@details  You can use this function to easily draw a combined.histogram and restricted cubic spline.The function draws the graph through ggplot2.RCS fitting requires the use of the rcs function of the RMS package.Can fit cox regression,logistic regression and linear regression models.
#'
#'
#'@param data need a dataframe
#'@param fit  You need the fitted model.Must be  lrm or coxph.
#'@param x The target variable you wish to fit. It is displayed on the X-axis when plotting.
#'@return a picture
#'@export
#'@examples library(rms)
#'library(ggplot2)
#'library(scales)
#'dt<-smoke
#'dd<-datadist(dt)
#'options(datadist='dd')
#'fit <- cph(Surv(time,status==1) ~ rcs(age,4), x=TRUE, y=TRUE,data=dt)
#'ggrcs(data=dt,fit=fit,x="age")


ggrcs<-function(data,fit,x,histlimit=NULL,histbinwidth=NULL,histcol=NULL,
                linetype=NULL,linesize=NULL,ribalpha=NULL,ribcol=NULL,xlab=NULL,ylab=NULL,
                subtitle=NULL,caption=NULL,leftaxislimit=NULL,lift=TRUE,
                scale_y_continuous=NULL,title=NULL,...){
  if (missing(data)) {stop("data is miss.")}
  if (missing(fit)) {stop("fit is miss.")}
  if (length(x) < 1) { stop("No valid variables.")}
  if (is.data.frame(data) == FALSE) {
    stop("The data argument needs to be a data frame (no quote).")
  }
  call <- match.call()
  fit <- fit;assign("fit", fit)
  dt<-data
  x1<-x
  x<-dt[,x]
  Pre0 <-predata(fit=fit,variables=x1,y=x)
  Pre0<-as.data.frame(Pre0)
  ####data set
  d<-density(x)
  dmin<-as.numeric(min(d[["y"]]))##density
  dmax<-as.numeric(max(d[["y"]]))##density
  yminlower<-as.numeric(min(Pre0$lower))
  ymaxupper<-as.numeric(max(yhat1<-Pre0$upper))
  #####ggplot set
  if (missing(histlimit)) {histlimit<-c(yminlower,ymaxupper)} else {assign("histlimit",histlimit)}
  if (missing(histbinwidth)) {histbinwidth<-0.8} else {assign("histbinwidth",histbinwidth)}
  if (missing(histcol)) {histcol<-"green"} else {assign("histcol",histcol)}
  if (missing(linetype)) {linetype<-1} else {assign("linetype",linetype)}
  if (missing(linesize)) {linesize<-1} else {assign("linesize",linesize)}
  if (missing(ribalpha)) {ribalpha<-0.3} else {assign("ribalpha",ribalpha)}
  if (missing(ribcol)) {ribcol<-"red"} else {assign("ribcol",ribcol)}
  if (missing(xlab)) {xlab<-x1} else {assign("xlab",xlab)}
  if (missing(ylab)) {ylab<-"Outcome Prediction Incidence"} else {assign("ylab",ylab)}
  if (missing(leftaxislimit)) {leftaxislimit<-c(dmin,dmax)} else {assign("leftaxislimit",leftaxislimit)}
  if (missing(title)) {title<-"The relationship between the variable and the predicted probability"} else {assign("title",title)}
  P<-ggplot(Pre0,aes(x=x))+
    geom_histogram(aes(x=x,y =rescale(..density..,histlimit)),binwidth = histbinwidth,fill=histcol,colour="black")+
    geom_line(data=Pre0,aes(x,yhat),linetype=linetype,size=linesize,alpha = 0.9,colour="red")+
    geom_ribbon(data=Pre0,aes(ymin = lower, ymax = upper),alpha = ribalpha,fill=ribcol)+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
    xlab(xlab)+
    ylab(ylab)+
    labs(title=title,subtitle=subtitle,caption=caption)
  ###LIFT
  if (lift==TRUE) {
    P<-P+scale_y_continuous(sec.axis = sec_axis( ~rescale(.,leftaxislimit),
                                                 name = "Probability density"))
  }
  P
}
