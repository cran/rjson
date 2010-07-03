.First.lib <- function(lib, pkg) {
	library.dynam("rjson", pkg, lib)
  #cat("JSON for R\n")
}
