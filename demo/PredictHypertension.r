#
# Fits a GLM to predict hypertension in a population.
#
# Args:
#   popn:       The population to which the GLM will be fitted and evaluated.
#   train.frac: The fraction of the population used to fit the GLM.
#
# Returns:
#   A list that contains the fitted GLM (glm) and the ROC plot object (roc).
#
PredictHypertension <- function(popn, train.frac=0.10) {
  ht.output <- "ISHYPER"
  popn.rnd <- RandomSubset(popn, train.frac)
  ht.formula <- MakeFormula(popn, ht.output)
  ht.glm <- MakeGLM(popn.rnd$subset, ht.formula, family=binomial())$glm.all.Xs
  p <- PlotROC(popn.rnd$rest, ht.output, ht.glm)
  return(list(glm=ht.glm, roc=p))
}

