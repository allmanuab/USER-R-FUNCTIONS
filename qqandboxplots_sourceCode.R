## SEE C:\Users\Hunter\Desktop\Grad_School\Programming\R-SAS\Rstats\Graphics/function-for-multiple-qqplots-and-boxplots.R
##   FOR AN EXAMPLE OF USING THIS FUNCTION

## FUNCTION
qqandboxplots <- function(data, qqplots=TRUE, boxplots=TRUE, cat.2D.boxplots=FALSE, boxplot.cat.var=NULL, cont.vars, path="", titles=rep("", length(cont.vars)), na.rm=TRUE) {
  library(ggplot2)
  library(ggthemes)
  
  vars = data[, cont.vars]
  nm <- names(vars)
  
  # WARNING IF LENGTH OF TITLES IS NOT CORRECT
  if (titles[1]!="" & length(cont.vars)!=length(titles)) {warning("Number of Titles passed does not equal the number of variables passed")}
  # warnings for boxplot.cat.var (only 1 factor can be passed to it)
  if (cat.2D.boxplots==TRUE & is.null(boxplot.cat.var)) {warning("2D categorized boxplots requested with cat.2D.boxplots=TRUE, but no categorical variable was passed to boxplot.cat.var")}
  if (!is.null(boxplot.cat.var) & length(boxplot.cat.var) != 1) {warning("Only one categorical variable can be passed to the boxplot.cat.var option")}
  if (!is.character(boxplot.cat.var) & !is.factor(boxplot.cat.var)) {warning("The variable passed to boxplot.cat.var must be a factor or character")}
  
  ## BOXPLOTS
  # 1D / no categorical variable for x axis.
  if (boxplots==TRUE) {
    for (i in seq_along(nm)) {
      # open pdf file if path is passed
      if (path != "") {pdf(file=paste(path, nm[i], "-boxplot", ".pdf",sep=""))}
      # make the plot
      p <- ( ggplot(vars, aes_string(x=1,y=nm[i])) + theme_tufte(base_size=16) +  
               geom_boxplot(outlier.colour="black", fill="lightblue",width=.4) +
               theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
               {if (titles[i]=="") labs(x = NULL, y=nm[i], title=paste(nm[i]," Boxplot",sep=""))
                 else labs(x = NULL, y=titles[i], title=paste("Boxplot of ", titles[i]))}  )
      print(p)
      # close path
      if (path != "") dev.off()
    }
  }
  # 2D / passed 1 categorical variable to be used on x axis of 
  if (cat.2D.boxplots==TRUE & !is.null(boxplot.cat.var)) {
    vars.c = data[, c(cont.vars,boxplot.cat.var)]
    if (is.factor(data$boxplot.cat.var)) {
      data$boxplot.cat.var = as.character(data$boxplot.cat.var)
      cat("Changed boxplot.cat.var from factor to character")
    }
    for (i in seq_along(nm)) {
      # open pdf is path is passed
      if (path != "") {pdf(file=paste(path, nm[i],"-by-", boxplot.cat.var,"-boxplot", ".pdf",sep=""))}
      # make the plot
      p <- (ggplot(vars.c, aes_string(x=boxplot.cat.var,y=nm[i])) + theme_tufte(base_size = 16) +  
              geom_boxplot(outlier.colour="black", fill="lightblue",width=.4) +
              {if (titles[i]=="") labs(title=paste("Boxplot of ", nm[i], " by ", boxplot.cat.var, sep="")) 
                else labs(y=titles[i], title=paste("Boxplot of ", titles[i], " by ", boxplot.cat.var, sep=""))
              } )
      print(p)
      if (path != "") dev.off()
    }
  }
  
  ## QQPLOTS
  if (qqplots==TRUE) {
    for (i in seq_along(nm)) {
      # open path if it was passed
      if (path != "") {pdf(file=paste(path,nm[i],"-qqplot",".pdf",sep=""))}
      # qqplot and normal line
      if (titles[i]=="") {
        qqnorm(data[,nm[i]], main=paste("QQ-plot of ", nm[i], sep=""))
        qqline(data[,nm[i]], col="blue")
      } else {
        qqnorm(data[,nm[i]], main=paste("QQ-plot of ", titles[i], sep=""))
        qqline(data[,nm[i]], col="blue")
      }
      # close path if it was passed
      if (path != "") dev.off()
    }
  }
}

save(qqandboxplots, file="C:/Users/Hunter/Desktop/Grad_School/Programming/R-SAS/Rstats/USER-FUNCTIONS/qqandboxplots.RData")