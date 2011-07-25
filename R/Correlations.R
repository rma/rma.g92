#
# Determines which parameters or variables are most highly correlated with some
# value in a given data set.
#
# Args:
#   data:    The data set from which the correlations will be calculated.
#   value:   The value against which the correlations will be measured.
#   method:  Which correlation coefficient to compute (see "stats::cor").
#   count:   The number of values to be returned (ie, the top N). Set this to
#            NULL or a negative number to return all values.
#   examine: Whether to correlate parameters ("par") or variables ("var").
#
# Returns:
#   A list of the most correlated values in descending order. Each correlation
#   coefficient is tagged with the corresponding value name.
#
# Example:
#   data(smallppn)
#   most.corr.vars <- MostCorrelated(smallppn, "v_PA", count=-1, examine="var")
#
MostCorrelated <- function(data, value, method="spearman", count=10,
                           examine=c("par","var"), only.exps=c("pre", "post", "both")) {
  cols <- names(data)
  val.col <- which(cols == value)

  examine <- match.arg(examine)
  if (examine == "par") {
    input.cols <- grep("p_", cols)
  } else if (examine == "var") {
    input.cols <- grep("v_", cols)
  } else {
    stop(sprintf("Invalid argument for MostCorrelated: examine"))
  }
  input.cols <- input.cols[! input.cols == val.col]

  row.count <- dim(data)[1]
  only.exps <- match.arg(only.exps)
  if (only.exps == "pre") {
    row.range <- seq(from=1, to=row.count, by=2)
  } else if (only.exps == "post") {
    row.range <- seq(from=2, to=row.count, by=2)
  } else if (only.exps == "both") {
    row.range <- 1:row.count
  } else {
    stop(sprintf("Invalid argument for MostCorrelated: only.exps"))
  }

  data.vals <- data[val.col, row.range]
  data.cmps <- data[input.cols, row.range]

  cors <- cor(data.vals, data.cmps, method=method)
  cor.values <- c(cors)

  na.values <- is.na(cor.values)
  cor.values <- cor.values[! na.values]
  cor.names <- names(data[input.cols])[! na.values]

  sort.by <- abs(cor.values)
  sorted.ixs <- sort.int(sort.by, decreasing=TRUE, index.return=TRUE)$ix

  if (is.null(count) || count < 0 || count > length(sorted.ixs)) {
    count <- length(sorted.ixs)
  }
  result = as.list(cor.values[sorted.ixs[1:count]])
  names(result) <- cor.names[sorted.ixs[1:count]]
  return(result)
}

#
#
#
SignificantCorrelations <- function (popn, value, examine=c("par","var"),
    only.exps=c("pre", "post", "both")) {
  cols <- names(popn)
  val.col <- which(cols == value)

  examine <- match.arg(examine)

  if (examine == "par") {
    input.cols <- grep("p_", cols)
  } else if (examine == "var") {
    input.cols <- grep("v_", cols)
    skip.logical <- grep("v_IS", cols, invert=TRUE)
    input.cols <- intersect(input.cols, skip.logical)
  } else {
    stop(sprintf("Invalid argument for SignificantCorrelations: examine"))
  }
  input.cols <- input.cols[! input.cols == val.col]

  row.count <- dim(popn)[1]
  only.exps <- match.arg(only.exps)
  if (only.exps == "pre") {
    row.range <- seq(from=1, to=row.count, by=2)
  } else if (only.exps == "post") {
    row.range <- seq(from=2, to=row.count, by=2)
  } else if (only.exps == "both") {
    row.range <- 1:row.count
  } else {
    stop(sprintf("Invalid argument for SignificantCorrelations: only.exps"))
  }

  popn.val <- popn[row.range, val.col]

  if (length(unique(popn.val)) == 1) {
    warning(sprintf("Constant correlation variable: %s", value))
    return(list())
  }

  sign.corrs <- list()
  # suppress warnings
  op <- options(warn = (-1))
  for (ix in input.cols) {
    popn.cmp <- popn[row.range, ix]
    c <- cor.test(popn.val, popn.cmp, method="spearman")
    if (! is.na(c$p.value) && c$p.value <= 0.05) {
      cmp.name <- substring(cols[ix], 3)
      sign.corrs[[cmp.name]] <- c$estimate[["rho"]]
    }
  }
  # reset the warnings
  options(op)

  return(sign.corrs)
}

SignCorrsForExps <- function(popn, only.exps, examine=c("par","var")) {
	var.names <- grep("v_", names(popn), value=TRUE)
	corr.list <- list()
	for (v in var.names) {
	    print(v)
	    var.corrs <- SignificantCorrelations(popn, v, examine=examine,
	                                         only.exps=only.exps)
	    corr.list[[v]] <- var.corrs
	}

	return(corr.list)
}

SignCorrsAfterDelta <- function(popn, examine=c("par","var")) {
    return(SignCorrsForExps(popn, "post", examine))
}

SignCorrsBeforeDelta <- function(popn, examine=c("par","var")) {
    return(SignCorrsForExps(popn, "pre", examine))
}

#
# A partial-correlation function that is an alternative to pcor.test().
#
pcor2 <- function(x1, x2) {
  # The canonical correlation matrix of x1 given x2.
  S1.2 <- cov(x1) - cov(x1, x2) %*% solve(cov(x2)) %*% cov(x2, x1)
  # The partial covariance of x1 given x2.
  D1.2.Inv <- diag(1 / sqrt(diag(S1.2)))
  # The partial correlations.
  partial <- D1.2.Inv %*% S1.2 %*% D1.2.Inv

  return(partial)
}

#
# Calculates the partial correlations against the given output, controlling for
# some parameters.
#
# Args:
#   data:     The data frame.
#   controls: The names of any control parameters.
#   output:   The output against which the correlations are calculated.
#
# Returns:
#   A list containing the estimated correlations (estimates), the p-values for
#   each estimate (pvalues) and the names of the correlated values (names).
#
PartialCorrelations <- function(data, controls, output) {
  output.var <- paste("v_", output, sep="")

  allnames <- grep("p_", names(data), value=TRUE)
  ns <- c()
  for (n in allnames) {
    if (! any(n == c(controls, output.var)))
      ns <- c(ns, n)
  }

  tmp = list(estimates=c(), pvalues=c(), names=ns, output=output)
  i <- 1
  for (n in ns) {
    pc <- pcor.test(data[[n]], data[[output.var]], data[controls], method="s")
    print(sprintf("%d out of %d", i, length(ns)))
    i <- i + 1
    tmp$estimates <- c(tmp$estimates, pc$estimate)
    tmp$pvalues <- c(tmp$pvalues, pc$p.value)
  }
  return(tmp)
}

#
# Plots the estimated partial correlations with some output.
#
# Args:
#   pcors:     The partial correlations as calculated by PartialCorrelations().
#   threshold: The maximum partial correlation to include in the plot.
#
# Returns:
#   The plot object.
#
PlotPartialCorr <- function(pcors, threshold = 1) {
  ys <- pcors$estimates
  ys[pcors$pvalues >= threshold] <- NA
  xs <- 1:length(ys)
  ls <- pcors$names
  ls[pcors$pvalues >= threshold] <- ""

  p <- qplot() +
    geom_point(aes(x = xs, y = ys, label = ls)) +
    geom_text(aes(size = 4, x = xs, y = ys, label = ls, hjust = -0.15)) +
    scale_x_continuous(limits=c(0,length(xs)*1.1)) +
    xlab("Parameters") +
    ylab(paste("Partial Correlation with", pcors$output)) +
    opts(legend.position = "none") +
    opts(axis.title.x = theme_text(size = 16)) +
    opts(axis.title.y = theme_text(size = 16, angle = 90)) +
    opts(axis.text.x = theme_text(size = 10, vjust = 1, colour = "#8F8F8F")) +
    opts(axis.text.y = theme_text(size = 10, hjust = 1, colour = "#8F8F8F"))

  return(p)
}

#
# Plots the p-values for the estimated partial correlations.
#
# Args:
#   pcors:     The partial correlations as calculated by PartialCorrelations().
#   threshold: The maximum partial correlation to include in the plot.
#
# Returns:
#   The plot object.
#
PlotPartialCorrPVals <- function(pcors, threshold = 1) {
  ys <- pcors$pvalues[pcors$pvalues < threshold]
  xs <- 1:length(ys)
  ls <- pcors$names[pcors$pvalues < threshold]

  p <- qplot() +
    geom_point(aes(x = xs, y = ys, label = ls)) +
    geom_text(aes(size = 4, x = xs, y = ys, label = ls, hjust = -0.15)) +
    scale_x_continuous(limits=c(0,length(xs)+2)) +
    xlab("Parameters") +
    ylab("P values") +
    scale_y_log10() +
    opts(legend.position = "none") +
    opts(axis.title.x = theme_text(size = 16)) +
    opts(axis.title.y = theme_text(size = 16, angle = 90)) +
    opts(axis.text.x = theme_text(size = 10, vjust = 1, colour = "#8F8F8F")) +
    opts(axis.text.y = theme_text(size = 10, hjust = 1, colour = "#8F8F8F"))

  return(p)
}

#
# An example of using PlotPartialCorr() and PlotPartialCorrPVals() to examine
# the relationships between some output and the rest of a data set.
#
# Args:
#   data:     The data set.
#   controls: The values to control for, if any.
#   output:   The name of the output to correlate against.
#
# Returns:
#   A list of six plot objects; pairs of partial correlation plots and plots of
#   the associated p-values, for three different threshold levels.
#
ExamplePartialCorrPlots <- function(data, controls, output) {
  plots = list()
  pcors <- PartialCorrelations(data, controls, output="PA")

  plots$pc1 <- PlotPartialCorr(pcors, threshold = 1)
  plots$pc2 <- PlotPartialCorr(pcors, threshold = 1e-2)
  plots$pc3 <- PlotPartialCorr(pcors, threshold = 1e-3)
  plots$pv1 <- PlotPartialCorrPVals(pcors, threshold = 1)
  plots$pv2 <- PlotPartialCorrPVals(pcors, threshold = 1e-2)
  plots$pv3 <- PlotPartialCorrPVals(pcors, threshold = 1e-3)

  return(plots)
}
