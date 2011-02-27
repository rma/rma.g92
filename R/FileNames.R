#
# Determines the name of the data file that contains the fitted GLM for a given
# output in a given population.
#
# Args:
#   popn.prefix: The prefix that uniquely identifies the population.
#   output.name: The name of the output to which the GLM was fitted.
#
# Returns:
#   The name of the data file containing the appropriate GLM.
#
DataFileName <- function(popn.prefix, output.name) {
  return(paste("RData.glm.", popn.prefix, ".", output.name, sep=""))
}

#
# Determines the filenames to which plots of a given GLM will be saved.
#
# Args:
#   popn.prefix: The prefix that uniquely identifies the population.
#   output.name: The name of the output to which the GLM was fitted.
#
# Returns:
#   The filename pattern, including a numerical specified (eg, "%d").
#
PlotFileName <- function(popn.prefix, output.name) {
  return(paste("glm", popn.prefix, output.name, "plot", "%03d", "pdf", sep="."))
}

#
# Determines the path of the directory that will store the plots of a GLM that
# was fitted to some output in a given population.
#
# Args:
#   popn.prefix: The prefix that uniquely identifies the population.
#   output.name: The name of the output to which the GLM was fitted.
#
# Returns:
#   The name of the directory where the plots should be saved.
#
DestDirName <- function(popn.prefix, output.name) {
  top.dir <- "virtppl_analysis"
  popn.dir <- paste("virtppl.", popn.prefix, sep="")
  glm.dir <- paste("glm.", popn.prefix, ".", output.name, sep="")
  return(file.path(top.dir, popn.dir, glm.dir))
}

