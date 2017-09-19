#' @description `.argMaster`
#' @param nam A character - a function name. One of `plot.mgcv.smooth.1D`, `plot.mgcv.smooth.2D`, 
#' `plot.fs.interaction`.
#' @return A list, with default options and graphical parameters.
#' @noRd
.argMaster <- function(nam){
  nam <- match.arg(nam, c("plot.mgcv.smooth.1D", "plot.mgcv.smooth.2D", "plot.fs.interaction"))
  out <- switch(nam,
                "plot.mgcv.smooth.1D" = list(
                  "a.rug" = list(jit = FALSE, colour = "black", size = 0.2, alpha = 1), # rug layer
                  "a.ci" = list(shade = TRUE, se.mult = 2, unconditional = FALSE, seWithMean = FALSE),
                  "a.cilin" = list(colour = "blue", linetype = "dashed"), # ci lines layer
                  "a.cipoly" = list(fill = "light grey", colour = "light blue", alpha = 1), # ci shade layer
                  "a.res" = list(by.resids = FALSE, colour = "black", shape = 46, na.rm = TRUE), # residuals layer
                  "a.dens" = list(ngr = c(50, 50), bw = NULL, tol = 1e-6, alpDen = 0.7, 
                                  colours = viridis(50, begin = 0.2), na.value = "white", 
                                  dTrans = sqrt) # density layer
                ),
                "plot.mgcv.smooth.2D" = list(),
                "plot.fs.interaction" = list())
  return(out)
}
