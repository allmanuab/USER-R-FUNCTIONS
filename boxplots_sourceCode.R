## SEE C:\Users\Hunter\Desktop\Grad_School\Programming\R-SAS\Rstats\Graphics/function-for-multiple-boxplots.R
##   FOR AN EXAMPLE OF USING THIS FUNCTION

boxplots <- function(data, cont.vars, path="", titles=rep("", length(cont.vars)), na.rm=TRUE) {
  library(ggplot2)
  library(ggthemes)
  
  boxplot.vars = data[,cont.vars]
  nm <- names(boxplot.vars)
  
  if (titles[1]!="" & length(cont.vars)!=length(titles)) {warning("Number of Titles passed does not equal the number of variables passed")}
  
  for (i in seq_along(nm)) {
    # open pdf file if path is passed
    if (path != "") {pdf(file=paste(path, nm[i], ".pdf",sep=""))}
    
    # make the plot
    p <- (ggplot(boxplot.vars, aes_string(x=1,y=nm[i])) + theme_tufte(base_size = 16) +  
            geom_boxplot(outlier.colour="black", fill="lightblue",width=.4) +
            theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
            {if (titles[i]=="") labs(x = NULL, y=nm[i], title=paste("Boxplot of ", nm[i],sep=""))
              else labs(x = NULL, y=titles[i], title=paste("Boxplot of ", titles[i], sep=""))
            }
    )
    
    print(p)
    
    if (path != "") dev.off()
  }
}

save(boxplots, file="C:/Users/Hunter/Desktop/Grad_School/Programming/R-SAS/Rstats/USER-FUNCTIONS/boxplots.RData")