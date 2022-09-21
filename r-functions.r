################################################################################
##CREATED BY: Marcel van Willligen, Data Manager
##
##CREATED ON: 21-Sep-2022
##
##LAST MODIFIED: 21-Sep-2022
##
##STATUS: In Development
##
##FILENAME: r-functions.R
##
##DESCRIPTION: Functions that are used in multiple scripts
## 
################################################################################

##%notin%---------------
##Create function to remove unwanted files using '%notin%' as a counterpart of %in%
`%notin%` <- function(x,y) {!(x %in% y)}

##dupsBetweenGroups----------
##Function for comparing old/new dataset from R Cookbook, datasets need to be
##combined and using an additional column to seperate out the different datasets
dups.betweengroups <- function (df, idcol) {
  # df: the data frame
  # idcol: the column which identifies the group each row belongs to
  
  # Get the data columns to use for finding matches
  datacols <- setdiff(names(df), idcol)
  
  # Sort by idcol, then datacols. Save order so we can undo the sorting later.
  sortorder <- do.call(order, df)
  df <- df[sortorder,]
  
  # Find duplicates within each id group (first copy not marked)
  dupWithin <- duplicated(df)
  
  # With duplicates within each group filtered out, find duplicates between groups. 
  # Need to scan up and down with duplicated() because first copy is not marked.
  dupBetween = rep(NA, nrow(df))
  dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols])
  dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols], fromLast=TRUE) | dupBetween[!dupWithin]
  
  # ============= Replace NA's with previous non-NA value ==============
  # This is why we sorted earlier - it was necessary to do this part efficiently
  
  # Get indexes of non-NA's
  goodIdx <- !is.na(dupBetween)
  
  # These are the non-NA values from x only
  # Add a leading NA for later use when we index into this vector
  goodVals <- c(NA, dupBetween[goodIdx])
  
  # Fill the indices of the output vector with the indices pulled from
  # these offsets of goodVals. Add 1 to avoid indexing to zero.
  fillIdx <- cumsum(goodIdx)+1
  
  # The original vector, now with gaps filled
  dupBetween <- goodVals[fillIdx]
  
  # Undo the original sort
  dupBetween[sortorder] <- dupBetween
  
  # Return the vector of which entries are duplicated across groups
  return(dupBetween)
}

##sem---------
##Calculate the standard error of the mean for x
sem <- function(x) {sd(x)/sqrt(length(x))}