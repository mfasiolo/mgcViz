
.onLoad <- function(libname = find.package("grattan"), pkgname = "grattan"){
  
  # CRAN Note avoidance
  if(getRversion() >= "2.15.1") 
    utils::globalVariables(
      # sample file names from taxstats
      c("x", "x", "y", "z", "sx", "sy", 
        "resx", "resy", "id", "response", "uci", "lci", "d", 
        "tz", "ty", "gx", "gy", "gid", "ll", "ul", "lo", "la", 
        "qu")
    )
  invisible()
}

# .onUnload <- function (libpath) {
#   library.dynam.unload("mgcViz", libpath)
# }
