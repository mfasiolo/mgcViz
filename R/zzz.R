
.onLoad <- function(...){
  
  # CRAN Note avoidance
  if(getRversion() >= "2.15.1") 
    utils::globalVariables(
      # sample file names from taxstats
      c("x", "x", "y", "z", "sx", "sy", 
        "resx", "resy", "id", "response", "uci", "lci", "d", 
        "tz", "ty", "gx", "gy", "gid", "ll", "ul", "lo", "la", 
        "qu", "p")
    )
  invisible()
  
}

.onAttach <- function(...){
  
  if( packageVersion("qgam") < "1.3.0" ){
    packageStartupMessage("qgam version < 1.3.0 installed, argument err will be set to 0.05 in qgamV and mqgamV calls. \n Update to qgam 1.3.0 for automatic err selection")
  }
  
}

# .onUnload <- function (libpath) {
#   library.dynam.unload("mgcViz", libpath)
# }
