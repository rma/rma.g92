#
# .Rprofile
#
# Automatically source R files from my personal directory.
#

#
# Loads required libraries and also source files from a particular directory.
#
# Args:
#   reload.libs: Whether to load the libraries.
#   src.path:    The path to the directory containing the source files. Leave
#                this as NULL to use the default directory.
#
# Returns:
#   Nothing.
#
.First <- function(reload.libs=TRUE, src.path=NULL) {
  if (is.null(src.path)) {
    src.path <- file.path(Sys.getenv("HOME"), "src", "guyton", "rma.g92", "R")
  }

  src.files <- Sys.glob(file.path(src.path, "*.r"))
  for (Rfile in src.files) {
    print(Rfile)
    source(Rfile)
  }

  if (reload.libs) {
    library(ggplot2)
    library(Hmisc)
    library(ROCR)
  }
}

#
# Reload the source files located in a particular directory.
#
# Args:
#   reload.libs: Whether to reload associated libraries as well.
#   src.path:    The path to the directory containing the source files. Leave
#                this as NULL to use the default directory.
#
# Returns:
#   Nothing.
#
reload.source <- function(reload.libs=FALSE, src.path=NULL) {
  .First(reload.libs=reload.libs, src.path)
}

#
# Quits the R session without being prompted to save the workspace.
#
q <- function() {
  quit(save="no")
}

