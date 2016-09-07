is_outlier <- function(x, na.rm=TRUE) {
  return(x < quantile(x, 0.25, na.rm=T) - 1.5 * IQR(x, na.rm=T) | x > quantile(x, 0.75, na.rm=T) + 1.5 * IQR(x, na.rm=T))
}

save(is_outlier, file="C:/Users/Hunter/Desktop/Grad_School/Programming/R-SAS/Rstats/USER-FUNCTIONS/is_outlier.RData")
save(is_outlier, file="C:/Users/Hunter/Desktop/Grad_School/Research/Mehta/Analysis/crossroads/src/R-functions/is_outlier.RData")