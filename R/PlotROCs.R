#
# Calculates the performance of a binomial GLM on a given data frame, with
# respect to some performance characteristics.
#
# Args:
#   glm:       The binomial GLM whose performance will be evaluated.
#   data:      The data frame on which to evaluate the GLM's performance.
#   output:    The name of the binomial output.
#   measure:   The y-axis performance measure (see ROCR::performance).
#   x.measure: The x-axis performance measure (see ROCR::performance).
#
# Returns:
#   A list containing the performance details (perf) and the area under curve
#   (auc) for the ROC curve of the GLM predictions.
#
GLMPerformance <- function(glm, data, output, measure="tpr", x.measure="fpr") {
  v_output <- paste("v_", output, sep="")
  glm.preds <- predict(glm, data)
  pred <- prediction(glm.preds, data[v_output])
  perf <- performance(pred, measure=measure, x.measure=x.measure)
  auc <- performance(pred, "auc")@y.values[[1]]

  return(list(perf=perf, auc=auc))
}

#
# Plots the result of using a binomial GLM to predict a binary output (such as
# a threshold).
#
# Args:
#   data:   The set of inputs and output from which predictions will be made.
#   output: The name of the binary output to be predicted by the GLM.
#   glm:    The binomial GLM to evaluate.
#
# Returns:
#   The plot object for the ROC curve.
#
# Example:
#   data(smallppn)
#   rndppn <- RandomSubset(smallppn, 0.10)
#   ht.frm <- MakeFormula(smallppn, "ISHYPER")
#   ht.glm <- MakeGLM(rndppn$subset, ht.frm, family=binomial())$glm.all.Xs
#   p <- PlotROC(rndppn$rest, "ISHYPER", ht.glm)
#   print(p)
#
PlotROC <- function(data, output, glm) {
  perf.list <- GLMPerformance(glm, data, output)
  perf <- perf.list$perf
  auc <- perf.list$auc

  auc_lbl = sprintf("Area = %0.3f", auc)
  p <- qplot() +
    geom_line(aes(x=perf@x.values[[1]], y=perf@y.values[[1]], weight=1,
                  colour="all")) +
    geom_text(aes(size = 5, x = 0.5, y = 0.5, label = auc_lbl)) +
    xlab("False positive rate") +
    ylab("True positive rate") +
    scale_x_continuous(limits=c(0,1)) +
    scale_y_continuous(limits=c(0,1)) +
    opts(legend.position = "none")

  return(p)
}

#
# Plots multiple ROCs on the one graph.
#
# Args:
#   data:   The inputs and output from which predictions will be made.
#   output: The name of the binary output to be predicted by the GLMs.
#   glms:   A list of binomial GLMs. Note that each GLM may have a field
#               named "label" for identifying the AUC labels on the plot.
#   title:  The title of the plot legend.
#   label:  A function for calculating ROC labels, of the form
#           "function(glm, auc, i)" for the ith GLM.
#   mono:   Whether to produce a monochrome or colour plot.
#
# Returns:
#   The plot object.
#
PlotROCs <- function(data, output, glms, title=NULL, label=NULL, mono=FALSE) {
  # The false-positive rates for each ROC.
  xs <- c()
  # The true-positive rates for each ROC.
  ys <- c()
  # The ROC label for each point in the data frame.
  lbls <- c()
  # The set of unique ROC labels.
  lbls.factors <- c()
  # The loop counter for numbering the GLMs.
  i <- 1

  for (glm in glms) {
    # Calculate the performance of the GLM on the data set.
    perf.list <- GLMPerformance(glm, data, output)
    perf <- perf.list$perf
    auc <- perf.list$auc

    # Extract the false-positive and true-positive rates.
    xs <- c(xs, perf@x.values[[1]])
    ys <- c(ys, perf@y.values[[1]])

    # Determine the label for this ROC.
    if (! is.null(legend.txt)) {
      auc.lbl = legend.txt(glm, auc, i)
    } else if (! is.null(glm$label)) {
      auc.lbl = sprintf("%s (%0.3f)", glm$label, auc)
    } else {
      auc.lbl = sprintf("GLM #%d (%0.3f)", i, auc)
    }

    # Update the data-point labels.
    lbls <- c(lbls, rep(auc.lbl, length(perf@x.values[[1]])))
    # Update the list of unique labels.
    lbls.factors <- c(lbls.factors, auc.lbl)

    # Increment the loop counter.
    i <- i + 1
  }

  # Determine the legend title.
  if (! is.null(title)) {
    legend.title <- title
  } else {
    legend.title <- "GLM (Area)"
  }

  # Select the appropriate colour scale.
  if (mono) {
    colour.scale <- scale_colour_grey(name=legend.title, start=1, end=1)
  } else {
    colour.scale <- scale_colour_hue(name=legend.title)
  }

  # Create the data frame for the plot.
  roc.data <- data.frame(FPR = xs, TPR = ys, ROC = lbls)

  # Plot the ROCs.
  p <- qplot(data=roc.data) + geom_text(size = 10) +
       geom_line(aes(x = FPR, y = TPR, weight = 0.5,
                     colour = factor(ROC, levels=lbls.factors))) +
       xlab("False positive rate") +
       ylab("True positive rate") +
       scale_x_continuous(limits=c(0,1)) +
       scale_y_continuous(limits=c(0,1)) +
       colour.scale +
       opts(legend.title = theme_text(size = 12, hjust=0)) +
       opts(axis.title.x = theme_text(size = 16)) +
       opts(axis.title.y = theme_text(size = 16, angle = 90)) +
       opts(axis.text.x = theme_text(size = 10, vjust = 1,
            colour = "#8F8F8F")) +
       opts(axis.text.y = theme_text(size = 10, hjust = 1,
            colour = "#8F8F8F"))

  return(p)
}

#
# Plots a bar graph showing the trade-off between true-positives and
# false-positives when different thresholds are applied to a binomial GLM.
#
# Args:
#   perf: The performance characteristics of the binomial GLM.
#   fpr:  A list of false-positive rates at which to examine the trade-off.
#   pos.popn: The number of cases for which the actual outcome is positive.
#   neg.popn: The number of cases for which the actual outcome is negative.
#
# Returns:
#   The plot object.
#
PlotBarGraphOfROC <- function(perf, fpr, pos.popn, neg.popn) {
  fpr.len <- length(fpr)

  fpr_i = rep(0, fpr.len)
  for (i in seq.int(1, fpr.len)) {
    fpr_i[i] <- which(as.numeric(perf@x.values[[1]]) > fpr[i], arr.ind=TRUE)[1]
  }

  tpr <- as.numeric(perf@y.values[[1]])[fpr_i]

  true.pos <- tpr * pos.popn
  false.neg <- true.pos - pos.popn
  false.neg2 <- (1-tpr) * pos.popn
  false.pos <- fpr * neg.popn

  for (i in seq.int(1, fpr.len)) {
    print(c(fpr[i], tpr[i], true.pos[i], false.neg[i], false.pos[i]))
  }

  df <- data.frame(
        fpr=rep(fpr * 100, 3),
        result=c(rep("True positive", fpr.len), rep("False negative", fpr.len),
                 rep("False positive", fpr.len)),
        y=c(true.pos, abs(false.neg), false.pos))

  p <- qplot(y=y, factor(fpr), data=df, geom="bar", fill=factor(result)) +
       labs(fill="Outcomes") +
       xlab("False positive rate (%)") +
       ylab("Predictions") +
       opts(plot.background = theme_rect(fill=NA, col=NA),
            legend.background = theme_rect(fill=NA, col=NA)) #+

  return(p)
}
