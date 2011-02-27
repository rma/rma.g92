#
# Constructs a formula for fitting a linear model to a set of observations.
#
# Args:
#   data:   The data frame that contains the model observations.
#   output: The name of the output variable in the data frame.
#
# Returns:
#   A formula that can be used to fit a GLM to the observations.
#
MakeFormula <- function(data, output) {
  output <- paste("v_", output, sep="")
  cols <- names(data)
  out.col <- which(cols == output)
  in.cols <- grep("p_", cols)
  model.output = data[out.col]
  model.inputs = data[in.cols]

  in.names <- paste(cols[in.cols], collapse="+")
  return( as.formula(paste(output, " ~ ", in.names)) )
}

#
# Fits a linear model to a set of observations.
#
# Args:
#   data:    The data frame that contains the model observations.
#   formula: The formula for the linear model.
#   step:    Select a reduced model by AIC (default is FALSE).
#   family:  The error distribution and link function (default is gaussian).
#
# Returns:
#   A list containing the fitted linear model (glm.all.Xs), and the reduced
#   model (glm.step.Xs) if step is TRUE. Both models inherit from the "glm"
#   class; see the documentation for "glm" for further details.
#
MakeGLM <- function(data, formula, step=FALSE, family=gaussian()) {
  glm.all.Xs <- glm(formula, family=family, data)
  result = list(glm.all.Xs = glm.all.Xs)

  if (step) {
    glm.step.Xs <- step(glm.all.Xs, trace = 0)
    result$glm.step.Xs <- glm.step.Xs
  }

  return(result)
}

#
# Determines to which parameters a GLM exhibits the greatest sensitivity, for
# a given data set.
#
# Args:
#   glm:    The GLM to be examined.
#   data:   The data set over which the sensitivity will be calculated.
#   output: The output that is predicted by the GLM.
#   count:  The number of parameters to be returned (ie, the top N). Set this
#           to NULL or a negative number to return all parameters.
#
# Returns:
#   A list of the most sensitive parameters in descending order. Each item is
#   tagged with the parameter name and corresponds to the effect of the
#   parameter on the GLM estimate (ie, 4 standard deviations).
#
# Example:
#   data(smallppn)
#   rndppn <- RandomSubset(smallppn, 0.10)
#   pa.frm <- MakeFormula(smallppn, "PA")
#   pa.glm <- MakeGLM(rndppn$subset, pa.frm)$glm.all.Xs
#   top.params <- MostSensitiveParameters(pa.glm, smallppn, "PA")
#
MostSensitiveParameters <- function(glm, data, output, count=10) {
  output <- paste("v_", output, sep="")

  params <- names(data)
  input.names <- names(glm$coefficients[-1])
  params <- params[input.names]
  obs <- data[input.names]

  if (is.null(count) || count < 0) {
    count = length(input.names)
  }

  obs.mean <- mean(obs)
  obs.sdev <- sd(obs)
  obs.min <- apply(obs, 2, min)
  obs.max <- apply(obs, 2, max)

  coeffs <- as.vector(coefficients(glm)[-1])
  sdev <- as.vector(obs.sdev)

  if (length(coeffs) < 1) {
    return(NULL);
  }

  contribs <- coefficients(glm)[-1] * 4 * obs.sdev
  top.ranks <- which(rank(abs(contribs)) > (length(contribs) - count),
                     arr.ind=TRUE)

  s <- sort.int(abs(contribs[top.ranks]), decreasing=TRUE, index.return=TRUE)
  return(contribs[top.ranks][s$ix])
}

#
# An example of a list of outputs, for use with FitGLMsToOutputs().
#
example.outputs <- list(
  PA = list(type="gaussian", desc="Arterial Pressure", unit="mmHg"),
  NOD = list(type="gaussian", desc="Sodium Excretion", unit="mEg/min"),
  KOD = list(type="gaussian", desc="Potassium Excretion", unit="mEg/min"),
  VUD = list(type="gaussian", desc="Urinary Output", unit="L/min"),
  QLO = list(type="gaussian", desc="Left Ventricular Output", unit="L/min"),
  QRO = list(type="gaussian", desc="Right Ventricular Output", unit="L/min"),
  QAO = list(type="gaussian", desc="Aortic Blood Flow", unit="L/min"))

#
# Fit GLMs to some number of outputs.
#
# Args:
#   data:    The data frame.
#   outputs: The list of outputs to which GLMs will be fitted. Each output
#            must include a description (desc), the units of the output (unit)
#            and the GLM family (type), either "gaussian" or "binomial".
#   prefix:  The prefix that uniquely identifies the data frame (population).
#   step:    Whether to also return a stepwise-selected minimal GLM.
#
# Returns:
#   Nothing; the results are saved in external data files, as specified by
#   DataFileName().
#
FitGLMsToOutputs <- function(data, outputs, prefix, step=TRUE) {
  for (output in names(outputs)) {

    output.glmtype <- outputs[[output]]$type
    output.desc  <- outputs[[output]]$desc
    if (output.glmtype == "gaussian") {
      print(paste("Gaussian GLM for", output))
      family = gaussian()
    } else if (output.glmtype == "binomial") {
      print(paste("Binomial GLM for", output))
      family = binomial()
    } else {
      warning(paste("Unknown GLM type:", output.glmtype))
      next
    }

    formula <- MakeFormula(data, output)
    res <- MakeGLM(data, formula, step=step, family=family)

    glm.all.Xs.sens <-
      MostSensitive(res$glm.all.Xs, data, count=20, output=output)
    if (step) {
      glm.step.Xs.sens <-
        MostSensitive(res$glm.step.Xs, data, count=20, output=output)
    }

    output.most.corr <- MostCorrelated(data, output, count=20)

    result = list(
      prefix = prefix,
      output = output,
      output.most.corr = output.most.corr,
      output.glmtype = output.glmtype,
      output.desc = output.desc,
      glm.all.Xs = res$glm.all.Xs,
      glm.all.Xs.sens = glm.all.Xs.sens
      )

    if (step) {
      result$glm.step.Xs <- res$glm.step.Xs
      result$glm.step.Xs.sens <- glm.step.Xs.sens
    }

    save(result, file=DataFileName(prefix, output))

    rm(result, res, formula, output.most.corr)
    gc(verbose=FALSE)
  }
}

#
# Fit GLMs to outputs across multiple populations.
#
# Args:
#   popns:   The list of populations for which GLMs will be fitted. Each
#            population must include the data frame (data) and the unique
#            prefix that identifies the population (prefix).
#   outputs: The list of outputs to which GLMs will be fitted. Each output
#            must include a description (desc), the units of the output (unit)
#            and the GLM family (type), either "gaussian" or "binomial".
#   step:    Whether to also return a stepwise-selected minimal GLM.
#
# Returns:
#   Nothing; the results are saved in external data files, as specified by
#   DataFileName().
#
FitGLMsToPopulations <- function(popns, outputs, step=TRUE) {
  for (p in popns) {
    FitGLMsToOutputs(p$data, outputs, p$prefix, step=step)
  }
}

#
# Performs stepwise selection of minimal GLMs for some number of outputs.
#
# Args:
#   data:    The data frame.
#   outputs: The list of outputs to which GLMs will be fitted.
#   prefix:  The prefix that uniquely identifies the data frame (population).
#
# Returns:
#   None; the results are loaded from and saved to external files, as specified
#   by DataFileName().
#
StepReduceSavedGLMs <- function(data, outputs, prefix) {
  for (output in names(outputs))  {
    load(DataFileName(prefix, output))

    if (result$output.glmtype == "gaussian") {
      print(paste("Gaussian (step) GLM for", output))
    } else if (result$output.glmtype == "binomial") {
      print(paste("Binomial (step) GLM for", output))
    }

    result$glm.step.Xs <- step(result$glm.all.Xs, trace = 1)
    result$glm.step.Xs.sens <-
      MostSensitive(result$glm.steps.Xs, data, count=20, output=output)

    save(result, file=DataFileName(prefix, output))
  }
}
