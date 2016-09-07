tochars <- function(data, to.char.vars) {
  vars <- data[,to.char.vars]
  # warnings for wrong datatype of to.char.vars and if it does not appear in the dataset passed
  df.name <- deparse(substitute(data))
  for (i in 1:length(names(vars))) {
    if (!(is.integer(vars[,i]) | is.factor(vars[,i]))) {warning(paste(names(vars)[i], " is not an integer or factor, it is a", class(vars[,i])))}
    if (!(to.char.vars[i] %in% names(data))) {warning(paste(to.char.vars[i]," is not found in ", df.name))}
  }
  
  # change factors and integers to chars
  for (i in to.char.vars) {
    print(i)
    if (class(data[,i])=="integer") {data[,i] <- as.character(data[,i])}
    if (class(data[,i])=="factor") {data[,i] <- as.character(data[,i])}
  }
  # data2 = data
  return(data)
  
}

save(tochars, file="C:/Users/Hunter/Desktop/Grad_School/Programming/R-SAS/Rstats/USER-FUNCTIONS/tochars.RData")
save(tochars, file="C:/Users/Hunter/Desktop/Grad_School/Research/Mehta/Analysis/crossroads/src/R-functions/tochars.RData")
