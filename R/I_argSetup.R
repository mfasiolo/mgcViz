.argSetup <- function(iArg, oArg, aNam, verbose = TRUE)
{
  if(length(oArg))
  {
    oNam <- names(oArg)
    iNam <-  names(iArg)
    
    if (verbose && length(noNms <- oNam[!oNam %in% iNam])) {
      
      warning("unknown names in argument list '", aNam, "': ",  paste(noNms, collapse = ", "), sep = '')
      
    }
    
    if(length(oArg)){ iArg[oNam] <- oArg }
  }
  
  return(iArg)
}