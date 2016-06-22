#' build dataset
#' 
#' Function to build dataset from source sqlite database. Checks for existence 
#' of local copy in inst/extdata of sqlite database.  If it does not exist 
#' locally, downloads from dropbox (eventually move to zenodo or figshare) and 
#' adds to gitignore (too big to push).  Reads in and conducts data munging 
#' required for anlaysis version of dataset. Sticks resultant data as csv into 
#' inst/extdata
#' @import dplyr  
build_data <- function(){
  wqm1db <- src_sqlite("inst/extdata/WQM1.db", create = T)
  param <- tbl(wqm1db,"Parameter")
  
  
  
}