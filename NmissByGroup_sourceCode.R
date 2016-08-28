
NmissByGroup <- function(df, outcome, groupvar) {
    # df must be a dataframe
    # outcome must be passed as quoted (eg outcome="BloodPressure")
    # groupvar passed UNquoted
    library(gdata)
  
    dta = df; 
    
    ## set 0,1 indicator for missing outcome variable
    dta$missing.oc <- ifelse(is.na(dta[,outcome]), 1,0)
    attach(dta)
    
    # number of missings for each level of groupvar
    Nmiss.oc.group <- with(dta, aggregate(x=missing.oc, by=list(groupvar), FUN=sum))
    colnames(Nmiss.oc.group) <- c("GroupVar", "Nmiss")
    Nmiss.oc.group$GroupVar <- as.character(Nmiss.oc.group$GroupVar)
    
    # total number of obs for each level of groupvar
    Ntotal.sex <- aggregate(x=missing.oc, by=list(groupvar), FUN=nobs) # using nobs from gdata
    colnames(Ntotal.sex)[2] <- "N"
    
    # merge the two
    Nmiss.oc.group2 <- data.frame(Nmiss.oc.group$GroupVar, Ntotal.sex$N, Nmiss.oc.group$Nmiss)
    colnames(Nmiss.oc.group2) <- c("GroupVar","N","Nmiss")
    Nmiss.oc.group2$prop <- Nmiss.oc.group2$Nmiss / Nmiss.oc.group2$N
    
    detach(dta)
    
    return(Nmiss.oc.group2)

}

save(NmissByGroup, file="C:/Users/Hunter/Desktop/Grad_School/Programming/R-SAS/Rstats/USER-FUNCTIONS/NmissByGroup.RData")

