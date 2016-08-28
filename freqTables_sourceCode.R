
freqTables <- function(df, cat.vars) {
  freq.tabs <- list() # make a holder for the list of frequencies
  attach(df)
  for (i in cat.vars) { # loop thru each cat var and make a frequency table
    l = get(i)
    freq.tabs[[i]] <- table(l)
    freq.tabs
  }
  # unlist makes a poorly formatted df of levels, but can bring the into excel and make it pretty
  cat.vars.lvl.list <- as.data.frame(unlist(freq.tabs))
  NOTE <- "-> Poorly formatted, reformat and clean this in excel if presenting"
  
  detach(df)
  results <- list(freq.tabs = freq.tabs,
                 cat.vars.lvl.list = cat.vars.lvl.list,
                 Note = NOTE) 
  return(results)
}

