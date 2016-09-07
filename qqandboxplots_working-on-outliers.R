qqandboxplots <- function(data, qqplots=TRUE, boxplots=TRUE, cat.2D.boxplots=FALSE, boxplot.cat.var=NULL, cont.vars, path="", titles=rep("", length(cont.vars)), outlier.labels=FALSE, idvar="partid", na.rm=TRUE) {
  library(ggplot2)
  library(ggthemes)
  
  vars = data[, cont.vars]
  nm <- names(vars)
  df.name <- substitute(data)
  
  # warning if a variable/element of cont.vars is not found in data
    for (i in 1:length(cont.vars)) {
      if (!(cont.vars[i] %in% names(data))) {warning(paste(cont.vars[i], " is not found in ", df.name, sep="" ))}
    }  
  # WARNING IF LENGTH OF TITLES IS NOT CORRECT
    if (titles[1]!="" & length(cont.vars)!=length(titles)) {warning("Number of Titles passed does not equal the number of variables passed")}
  # warnings for boxplot.cat.var (only 1 factor can be passed to it)
    if (cat.2D.boxplots==TRUE & is.null(boxplot.cat.var)) {warning("2D categorized boxplots requested with cat.2D.boxplots=TRUE, but no categorical variable was passed to boxplot.cat.var")}
    if (!is.null(boxplot.cat.var) & length(boxplot.cat.var) != 1) {warning("Only one categorical variable can be passed to the boxplot.cat.var option")}
    if (!is.character(boxplot.cat.var) & !is.factor(boxplot.cat.var)) {warning("The variable passed to boxplot.cat.var must be a factor or character")}
  
  # make labels for outliers if requested
    if (outlier.labels==TRUE) {
      library(dplyr) # for mutate (Mutate adds new variables and preserves existing; transmute drops existing variables. Used in finding outliers)
      # warning if idvar is not in data
        if (!(idvar %in% names(data))) {warning(paste(idvar," is not found in ", df.name, sep=""))}
      # using id for labeling
        id <- data[,idvar]
      # returns true/false/NA if value is outside (75% / 25% Quartile) +- 1.5IQR
        is_outlier <- function(x, na.rm=TRUE) {
          return(x < quantile(x, 0.25, na.rm=T) - 1.5*IQR(x, na.rm=T) | x > quantile(x, 0.75, na.rm=T) + 1.5*IQR(x, na.rm=T))
        }
      # create variable with labels for outliers for each variable
        outlier.labs <- data.frame(matrix(NA, nrow = nrow(vars), ncol = ncol(vars)))
        for (i in seq_along(nm)) {
          vars[,paste("outlier.", nm[i], sep="")] <- ifelse(is_outlier(vars[,i]), paste(vars[,i], "; ", id, sep=""), as.numeric(NA))
          colnames(outlier.labs)[i] <- nm[i]  
          outlier.labs[, nm[i]] <- ifelse(is_outlier(vars[,i]), paste(vars[,i], "; ", id, sep=""), as.numeric(NA))
        }
    }
  
  ## BOXPLOTS
    ## 1D / no categorical variable for x axis.
    if (boxplots==TRUE) {
      for (i in seq_along(nm)) {
        # open pdf file if path is passed
          if (path != "") {pdf(file=paste(path, nm[i], "-boxplot", ".pdf",sep=""))}
        # make the plot
        p <- ( ggplot(vars, aes_string(x=1,y=nm[i])) + theme_tufte(base_size=16) +  
                 geom_boxplot(outlier.colour="black", fill="lightblue",width=.4) +
                 theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
                 {if (titles[i]=="") labs(x = NULL, y=nm[i], title=paste(nm[i]," Boxplot",sep=""))
                   else labs(x = NULL, y=titles[i], title=paste("Boxplot of ", titles[i])) } )
        # add labels to outliers if outlier.labels=TRUE
          if (outlier.labels==TRUE) {
            library(ggrepel)
            p <- p + geom_text_repel(aes(label=outlier.labs[,nm[i]]), nudge_x=.1, na.rm=TRUE) # hjust not supported
          }
        # return plots
          print(p)
        # close path
          if (path != "") dev.off()
      }
    }
    ## 2D / categorized boxplots
    if (cat.2D.boxplots==TRUE & !is.null(boxplot.cat.var)) {
      # define new df with the categorical variable passed to boxplot.cat.var
        vars.c = data[, c(cont.vars,boxplot.cat.var)]
      # change to char if factor
        if (is.factor(data$boxplot.cat.var)) {
          data$boxplot.cat.var = as.character(data$boxplot.cat.var)
          cat("Changed boxplot.cat.var from factor to character")
        }
      # warn that some outliers may be different from the 1D boxplots
        warning("Due to the changes in distribution when restricting to a level of a cat var, some outliers may be different than those defined using all values of the continuous variable")
      # make the plots  
        for (i in seq_along(nm)) {
          # open pdf is path is passed
            if (path != "") {pdf(file=paste(path, nm[i],"-by-", boxplot.cat.var,"-boxplot", ".pdf",sep=""))}
          # make the plot
            p <- (ggplot(vars.c, aes_string(x=boxplot.cat.var,y=nm[i])) + theme_tufte(base_size = 16) +  
                    geom_boxplot(outlier.colour="black", fill="lightblue",width=.4) +
                    {if (titles[i]=="") labs(title=paste("Boxplot of ", nm[i], " by ", boxplot.cat.var, sep="")) 
                      else labs(y=titles[i], title=paste("Boxplot of ", titles[i], " by ", boxplot.cat.var, sep=""))
                    } )
          # add labels to outliers if outlier.labels=TRUE
            if (outlier.labels==TRUE) {
              library(ggrepel)
              p <- p + geom_text_repel(aes(label=outlier.labs[,nm[i]]), nudge_x=.325, na.rm=TRUE) # hjust not supported
            }
          # return plots
            print(p)
          # close pdf if path was passed
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

# data <- xroads2
# cont.vars <- c("H_AOX_B", "L_AOX_B", "T_AOX_B", "TNFa_M0", "hsCRP_M0", "IL_6_M0", "LS_SOA_Scoring", "age_m0", "BaselineBMI_m0","HEI2010_TOTAL_SCORE")
# boxplot.cat.var <- "Gender_m0"
# rm(list=c("cont.vars","data","nm","idvar","df.name", "boxplot.cat.var"))
# rm(list="outlier.labs")
titles = c("Alternative Oxidase - H", "Alternative Oxidase - L", "Alternative Oxidase - T", "Tumor necrosis factor alpha", "high sensitivity C-reactive Protein", "Interleukin 6", "Lifespace Score", "Age at Baseline", "BMI at Baseline", "HEI-2010 Total Score")
path <- "C:/Users/Hunter/Desktop/Grad_School/Research/Mehta/Analysis/crossroads/results/preliminary-analyses/temp/"
cat.var = "Gender_m0"
#data, qqplots=TRUE, boxplots=TRUE, cat.2D.boxplots=FALSE, boxplot.cat.var=NULL, cont.vars, path="", titles=rep("", length(cont.vars)), outlier.labels=FALSE, idvar="partid", na.rm=TRUE
qqandboxplots(data=xroads2, qqplots=F, boxplots=T, cont.vars=cont.vars, path=path, titles=titles, outlier.labels=T, idvar="partid")

