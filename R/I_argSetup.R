###################################
#' @description When possible `.argSetup` overrides default arguments `iArg`
#'  by user defined values in `oArg`.
#' @param iArg A list, corresponding to one family of options, i.e `a.rug`.
#' @param oArg A list. Should be related to the same options.
#' @param aNam Name of the family of options : i.e `a.rug`.
#' @param verbose Should it print warning messages ?
#' @return A named list of values.
#' @noRd
.argSetup <- function(iArg, oArg, aNam, verbose = TRUE) {
  if (length(oArg)) {
    oNam <- names(oArg)
    iNam <- names(iArg)
    if (verbose && length(noNms <- oNam[!oNam %in% iNam])) {
      warning("unknown names in argument list '", aNam, "': ",
              paste(noNms, collapse = ", "), sep = '')
    }
    iArg[oNam] <- if(length(oArg)) oArg 
  }
  return(iArg)
}