outlier_dist_rm <- function(data, cont.var) {
  # identify, describe, plot and remove the outliers if it is necessary. To detect the outliers 
  # I use the command boxplot.stats()$out which use the Tukey's method to identify the outliers 
  # ranged above and below the 1.5*IQR. To describe the data I preferred to show the number (%) of outliers 
  # and the mean of the outliers in dataset. I also show the mean of data with and without outliers
  cont.var_name <- eval(substitute(cont.var),eval(data))
  na1 <- sum(is.na(cont.var_name))
  m1 <- mean(cont.var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(cont.var_name, main="With outliers")
  hist(cont.var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(cont.var_name)$out
  mo <- mean(outlier)
  cont.var_name <- ifelse(cont.var_name %in% outlier, NA, cont.var_name)
  boxplot(cont.var_name, main="Without outliers")
  hist(cont.var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(cont.var_name))
  cat("Outliers identified:", na2 - na1, "\n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(cont.var_name))*100, 1), "\n")
  cat("Mean of the outliers:", round(mo, 2), "\n")
  m2 <- mean(cont.var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "\n")
  cat("Mean if we remove outliers:", round(m2, 2), "\n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    data[as.character(substitute(cont.var))] <- invisible(cont.var_name)
    assign(as.character(as.list(match.call())$data), data, envir = .GlobalEnv)
    cat("Outliers successfully removed", "\n")
    return(invisible(data))
  } else{
    cat("Nothing changed", "\n")
    return(invisible(cont.var_name))
  }
}

save(outlier_dist_rm, file="C:/Users/Hunter/Desktop/Grad_School/Programming/R-SAS/Rstats/USER-FUNCTIONS/outlier_dist_rm.RData")
save(outlier_dist_rm, file="C:/Users/Hunter/Desktop/Grad_School/Research/Mehta/Analysis/crossroads/src/R-functions/outlier_dist_rm.RData")