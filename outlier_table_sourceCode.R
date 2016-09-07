outlier_table <- function(data, cont.vars=cont.vars, idvar="partid", path=NULL) {
  # get name of dataframe passed
    df.name <- deparse(substitute(data))
  # check that its a valid dataframe
    if (!exists(df.name) && !is.data.frame(get(df.name))) {warning("Must pass a valid dataframe to this function.")}
  # make df restricted to vars of interest
    vars <- data[,c(cont.vars, idvar)] 
  # some comments explaining what the function will do  
    if (!is.null(path)) {cat("If outliers are found, a table of their values and id's will be created\nand saved to a csv file. One file for each variable.")}
    if (is.null(path)) {cat("If outliers are found, a list will be created with number of elements equal to the length of cont.vars.\nThis list will contain the id's and values of each outlier,\nand a warning is given if no outliers are found in a particular variable.")}
  # create empty list with length of cont.vars
    outlier.list <- vector("list", length=length(cont.vars))
    names(outlier.list) <- cont.vars
  # save outliers from each cont.var to the list
    for (i in 1:length(cont.vars)) {
      # get values of outliers
      vals <- boxplot.stats(vars[,i])$out
      # warn if no outliers detected
      if (length(vals)==0) {warning(paste0("No outliers detected in ", cont.vars[i]))}
      # get ids of outliers
      ids <- vars[vars[,i] %in% vals, idvar]
      # combine vals and ids
      outliers <- cbind(ids, vals)
      # save to list
      outlier.list[[i]] <- outliers
      # remove element of list if it is empty / no outliers found
      outlier.list[[i]] <- ifelse(outlier.list[[i]]==0, outlier.list[[i]] <- NULL, outlier.list[[i]])
    } 
  ## make dataframe of outliers for each cont.var; 1:(length(names(vars))-1) to exclude "partid" at the end of vars
    # need this for the next loop for some reason, 
      nm <- names(vars[,1:(length(names(vars))-1)])
    # empty vector of names of outputted outlier dataframes, used to return them at the end of this function
      #outlier.tbls.vec <- vector("character", length=(length(names(vars))-1))
    # do the loop, and save to csv if path is passed
      for (i in seq_along(nm)) {
        # using assign to dynamically name dataframes
          #assign(paste0(nm[i], "_outliers"), as.data.frame(unlist(outlier.list[[i]])))
        # create char vec of the names of the dynamically named outlier dataframes
          #outlier.tbls.vec[i] <- paste0(nm[i], "_outliers")
        # save to csv if path is not null
          if (!is.null(path) & length(outlier.list[[i]])!=0) {
            write.csv(as.data.frame(unlist(outlier.list[[i]])), file=paste0(path, "/", paste0(nm[i], "_outliers"), ".csv"), row.names=F)
          }
      } # not sure how to return this from the function yet
      
  # return list of outliers
    outlier.list
 
}

save(outlier_table, file="C:/Users/Hunter/Desktop/Grad_School/Programming/R-SAS/Rstats/USER-FUNCTIONS/outlier_table.RData")
save(outlier_table, file="C:/Users/Hunter/Desktop/Grad_School/Research/Mehta/Analysis/crossroads/src/R-functions/outlier_table.RData")