#
# Calculates the significant of differences in parameter and variable values
# between two distinct populations, by using the Wilcoxon rank-sum test
# "stats::wilcox.test".
#
# Args:
#   names: The names of the parameters and/or variables to inspect.
#   popn1: The first population.
#   popn2: The second population.
#
# Returns:
#   A list with the tags "names", where each tagged value is a list with
#   class "htest" (see the "stats::wilcox.test" documentation for details).
#
# Example:
#   data(smallppn.nt)
#   data(smallppn.ht)
#   sign.params <- c("p_AARK", "p_ANPXAF", "p_CPR", "p_EARK", "p_EXC",
#     "p_EXCML", "p_FIS", "p_GFLC", "p_HM6", "p_HSL", "p_HSR", "p_LPPR",
#     "p_NID", "p_POR", "p_PXTP", "p_RNAUGN")
#   signs <- ParamSignificanceOverPopns(sign.params, smallppn.nt, smallppn.ht)
#
ParamSignificanceOverPopns <- function(names, popn1, popn2) {
  sig <- function(n) { return(wilcox.test(popn1[[n]], popn2[[n]])) }
  significances <- map(sig, names, conv=NULL)
  names(significances) <- names
  return(significances)
}
