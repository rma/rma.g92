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
    geom_line(aes(x=perf@x.values[[1]], y=perf@y.values[[1]], weight=0.5, colour="all")) +
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
#   data:   The set of inputs and output from which predictions will be made.
#   output: The name of the binary output to be predicted by the GLMs.
#   ...:    Any number of binomial GLMs. Note that each GLM should have a
#           field named "label" for identifying the AUC labels on the plot.
#
# Returns:
#   Nothing; the plot is printed to the active device.
#
PlotROCs <- function(data, output, ...) {
  p <- eval(qplot()) + geom_text(size = 10)
  i <- 1

  list.of.perfs <- list()
  list.of.aucs <- list()
  list.of.lbls <- list()
  list.of.colours <- list()

  for (glm in list(...)) {
    perf.list <- GLMPerformance(glm, data, output)
    perf <- perf.list$perf
    auc <- perf.list$auc

    list.of.perfs <- c(list.of.perfs, perf)
    list.of.aucs <- c(list.of.aucs, auc)

    if (! is.null(glm$label)) {
      auc_lbl = sprintf("Area (%s) = %0.3f", glm$label, auc)
    } else {
      auc_lbl = sprintf("Area (GLM #%d) = %0.3f", i, auc)
    }
    list.of.lbls <- c(list.of.lbls, auc_lbl)

    clr = as.character(i)
    list.of.colours <- c(list.of.colours, clr)

    p.geomline <- substitute(
        geom_line(aes(x=list.of.perfs[[j]]@x.values[[1]],
                      y=list.of.perfs[[j]]@y.values[[1]],
                      weight=0.5, colour=list.of.colours[[j]])),
        list(j=i))

    p.roclabel <- substitute(
        geom_text(aes(size=6, x=0.5, y=0.5-0.1*j, label=list.of.lbls[[j]],
                      colour=list.of.colours[[j]])),
        list(j=i))

    p <- p + eval(p.geomline) + eval(p.roclabel)

    i <- i + 1
  }

  p <- p +
    xlab("False positive rate") +
    ylab("True positive rate") +
    scale_x_continuous(limits=c(0,1)) +
    scale_y_continuous(limits=c(0,1)) +
    scale_colour_manual("", values=c("#CF005F", "#005FBF")) +
    opts(legend.position = "none") +
    opts(axis.title.x = theme_text(size = 16)) +
    opts(axis.title.y = theme_text(size = 16, angle = 90)) +
    opts(axis.text.x = theme_text(size = 10, vjust = 1, colour = "#8F8F8F")) +
    opts(axis.text.y = theme_text(size = 10, hjust = 1, colour = "#8F8F8F"))

  print(p)
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
