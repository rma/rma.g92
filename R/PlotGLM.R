#
# Produces a range of plots for evaluating how well a GLM fits a set of data.
#
# Args:
#   glm:      The GLM to be plotted.
#   data:     The data set against which the GLM will be evaluated.
#   output:   The name of the output in the data set.
#   to.plot:  Any subset of the sequence 1:7, identifying which plots to make.
#   axisname: A descriptive name of the output of the GLM.
#   axisunit: The units for the output of the GLM.
#   verbose:  Whether or not to print some progress information.
#
# Returns:
#   A list of plot objects.
#
# Example:
#   data(smallppn)
#   rndppn <- RandomSubset(smallppn, 0.10)
#   pa.frm <- MakeFormula(smallppn, "PA")
#   pa.glm <- MakeGLM(rndppn$subset, pa.frm)$glm.all.Xs
#   ps <- PlotGLM(pa.glm, smallppn, "PA", axisname="MAP", axisunit="mmHg")
#   print(ps)
#
PlotGLM <- function(glm, data, output, to.plot=1:7, axisname="", axisunit="",
                    verbose=TRUE) {
  ps <- list()

  output <- paste("v_", output, sep="")
  axislbl <- paste(axisname, " (", axisunit, ")", sep="")

  param.name <- grep("p_", names(coef(glm)), value=TRUE)

  param.count <- length(param.name)
  param.coeff <- coefficients(glm)[-1]
  obs <- data[param.name]
  obs.mean <- mean(obs)
  obs.sdev <- sd(obs)
  contr.mean <- param.coeff * obs.mean
  contr.sdev <- param.coeff * obs.sdev
  contr.m2sd <- param.coeff * (obs.mean - 2 * obs.sdev)
  contr.p2sd <- param.coeff * (obs.mean + 2 * obs.sdev)
  contr.range <- param.coeff * 4 * obs.sdev

  if (any(to.plot == 1)) {
    if (verbose) {
      print("GLM plot 1")
    }
    # plot the mean contributions, with labelled points
    p <- qplot(y = contr.mean, label = param.name) +
      # plot error bars
      geom_errorbar(aes(ymin = contr.m2sd, ymax = contr.p2sd)) +
      # label the X and Y axes
      scale_x_continuous("Model Parameter", breaks=1:param.count,
        labels=rep(c(""), param.count)) +
      scale_y_continuous(axislbl) +
      # set the font-size and position for the labelled points
      geom_text(size = 3, hjust = -0.3)

    ps$p1 <- p
  }
  if (any(to.plot == 2)) {
    if (verbose) {
      print("GLM plot 2")
    }
    # plot the range of contributions
    p <- qplot(y = abs(contr.range), x = rep(1, param.count),
               label = param.name) +
      # label the X and Y axes
      scale_x_continuous("Model Parameter") +
      ylab(expression(abs(delta[i]) ~~ (mmHg))) +
      # set the font-size and position for the labelled points
      geom_text(size = 3, hjust = -0.3)

    ps$p2 <- p
  }
  if (any(to.plot == 3)) {
    if (verbose) {
      print("GLM plot 3")
    }
    # plot the coefficients
    p <- qplot(y = param.coeff, x = rep(1, param.count), label = param.name) +
      # label the X and Y axes
      scale_x_continuous("Model Parameter") +
      scale_y_continuous("Coefficient") +
      # set the font-size and position for the labelled points
      geom_text(size = 3, hjust = -0.3)

    ps$p3 <- p
  }
  if (any(to.plot == 4)) {
    if (verbose) {
      print("GLM plot 4")
    }
    # plot the residuals against the fitted values
    p <- qplot(x = glm$fitted.values, y = glm$residuals) +
      geom_point() +
      geom_smooth() +
      geom_line(aes(y = rep(0, length(glm$fitted.values))), legend=FALSE) +
      xlab(paste("Fitted value (", axisunit, ")", sep="")) +
      ylab(paste("Residual (", axisunit, ")", sep="")) +
      opts(legend.position = c(0.85,0.875),
        legend.background=theme_rect(fill="white"),
        legend.title = theme_blank(),
        axis.title.x = theme_text(size = 14),
        axis.title.y = theme_text(size = 14, angle = 90),
        axis.text.x = theme_text(size = 10, vjust = 1, colour = "#8f8f8f"),
        axis.text.y = theme_text(size = 10, hjust = 1, colour = "#8f8f8f"))

    ps$p4 <- p
  }
  if (any(to.plot == 5)) {
    if (verbose) {
      print("GLM plot 5")
    }
    # histogram of the fitted values
    bw <- (max(glm$fitted.values) - min(glm$fitted.values)) / 100
    p <- qplot() +
      geom_density(aes(glm$fitted.values, y = ..density.., fill="red")) +
      geom_histogram(aes(glm$fitted.values, y = ..density.., alpha=0.67),
        binwidth = bw) +
      xlab(paste("Fitted value (", axisunit, ")", sep="")) +
      ylab("Density") +
      opts(legend.position="none")

    ps$p5 <- p
  }
  if (any(to.plot == 6)) {
    if (verbose) {
      print("GLM plot 6")
    }
    # histogram of the residuals
    bw <- (max(glm$fitted.values) - min(glm$fitted.values)) / 100
    p <- qplot() +
      geom_density(aes(glm$residuals, y = ..density.., fill="red")) +
      geom_histogram(aes(glm$residuals, y = ..density.., alpha=0.67),
        binwidth = bw) +
      xlab(paste("Residual (", axisunit, ")", sep="")) +
      ylab("Density") +
      opts(legend.position="none")

    ps$p6 <- p
  }
  if (any(to.plot == 7)) {
    if (verbose) {
      print("GLM plot 7")
    }
    p <- qplot() +
      geom_density(aes(data[[output]], fill = "Guyton"), alpha = 0.33) +
      geom_density(aes(glm$fitted.values, fill = "Linear"), alpha = 0.33) +
      xlab(paste(axisname, " (", axisunit, ")", sep="")) +
      ylab("Density") +
      opts(legend.position = c(0.9,0.9),
        legend.background=theme_rect(fill="white"),
        legend.title = theme_blank())

    ps$p7 <- p
  }

  return(ps)
}

#
# Plots a set of GLMs that have been fitted to a single population.
#
# Args:
#   data:    The data frame (ie, population) for the GLMs.
#   outputs: The outputs to which GLMs have been fitted.
#   prefix:  The prefix that uniquely identifies the population.
#
# Returns:
#   Nothing; the plots are saved to external files, see PlotFileName().
#
PlotPopulationGLMs <- function(data, outputs, prefix) {
  for (output in names(outputs))  {
    file.load <- DataFileName(prefix, output)
    file.plot <- PlotFileName(prefix, output)

    # Load the "result" list into the environment.
    print(paste("Loading file:", file.load))
    load(file.load)

    has.step = ("glm.step.Xs" %in% names(result))

    print("Plotting ...")
    # Export the plots as PDFs.
    cairo_pdf(filename=file.plot)

    # Create the plots.
    if (result$output.glmtype == "binomial") {
      v_output <- paste("v_", output, sep="")
      result$glm.all.Xs$label = "all params"

      if (has.step) {
        result$glm.step.Xs$label = "min params"
        roc <- PlotROCs(data[[v_output]], result$glm.all.Xs, result$glm.step.Xs)
      } else {
        roc <- PlotROCs(data[[v_output]], result$glm.all.Xs)
      }

      print(roc)
    } else {
      for (p in PlotGLM(result$glm.all.Xs, data, output=output)) {
        print(p)
      }
      if (has.step) {
        for (p in PlotGLM(result$glm.step.Xs, data, output=output)) {
          print(p)
        }
      }
    }

    # Plot the 20 most-sensitive and most-correlated parameters.
    count = 20
    most.sens.all <- MostSensitive(result$glm.all.Xs, data, output,
                                   count=count)
    p <- PlotListOfValues(most.sens.all, "Parameter", "Range")
    print(p)

    if (has.step) {
      most.sens.step <- MostSensitive(result$glm.steps.Xs, data, output,
                                      count=count)
      p <- PlotListOfValues(most.sens.step, "Parameter", "Range")
      print(p)
    }

    most.corr <- MostCorrelated(data, output, count=count, examine="par")
    p <- PlotListOfValues(most.corr, "Parameter", "Correlation")
    print(p)

    dev.off()

    print("Cleaning up ...")
    rm(result, roc)
    gc(verbose=TRUE)
  }
}
