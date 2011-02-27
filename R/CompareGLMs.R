#
# Makes a plot of the contributions (mean plus/minus two standard deviations)
# that each parameter makes to the output of two different GLMs.
#
# Args:
#   glm1:    The first of the GLMs to be compared.
#   data1:   The observations for the first GLM.
#   glm2:    The second of the GLMs to be compared.
#   data2:   The observations for the second GLM.
#   y.title: The title of the y-axis (ie, output name and units).
#
# Returns:
#   The plot object.
#
CompareGLMContributions <- function(glm1, data1, glm2, data2, y.title="") {
    names1 <- names(coef(glm1))[-1]
    count1 <- length(names1)
    coeffs1 <- coefficients(glm1)[-1]

    names2 <- names(coef(glm2))[-1]
    count2 <- length(names2)
    coeffs2 <- coefficients(glm2)[-1]

    max_count = max(count1, count2)

    obs1 <- data1[names1]
    obs1_mean <- mean(obs1)
    obs1_sdev <- sd(obs1)
    obs1_min <- apply(obs1, 2, min)
    obs1_min <- apply(obs1, 2, max)

    obs2 <- data2[names2]
    obs2_mean <- mean(obs2)
    obs2_sdev <- sd(obs2)
    obs2_min <- apply(obs2, 2, min)
    obs2_min <- apply(obs2, 2, max)

    c1_mean = coeffs1 * obs1_mean
    c1_sdev = coeffs1 * obs1_sdev
    c1_m1sd = coeffs1 * (obs1_mean - obs1_sdev)
    c1_m2sd = coeffs1 * (obs1_mean - 2 * obs1_sdev)
    c1_p1sd = coeffs1 * (obs1_mean + obs1_sdev)
    c1_p2sd = coeffs1 * (obs1_mean + 2 * obs1_sdev)

    c2_mean = coeffs2 * obs2_mean
    c2_sdev = coeffs2 * obs2_sdev
    c2_m1sd = coeffs2 * (obs2_mean - obs2_sdev)
    c2_m2sd = coeffs2 * (obs2_mean - 2 * obs2_sdev)
    c2_p1sd = coeffs2 * (obs2_mean + obs2_sdev)
    c2_p2sd = coeffs2 * (obs2_mean + 2 * obs2_sdev)

    # plot the mean contributions, with labelled points
    p <- ggplot(y = c1_mean, label = names1, colour="blue") +
       geom_point(aes(y = c2_mean), colour="red")
    # plot error bars
    p <- p +
      geom_errorbar(aes(ymin = c1_m1sd, ymax = c1_p1sd), colour="red") +
      geom_errorbar(aes(ymin = c2_m1sd, ymax = c2_p1sd), colour="blue")
    # label the X and Y axes
    p <- p + scale_x_continuous("Model Parameter", breaks=1:max_count,
         labels=rep(c(""), max_count))
    p <- p + scale_y_continuous(y.title)
    # set the font-size and position for the labelled points
    p <- p + geom_text(size = 3, hjust = -0.3)

    #p2 <- qplot(y = c1_range, label = names1, colour="red") +
    #  geom_point(aes(y = c2_range, colour="blue")) +
    #  geom_text(size = 3, hjust = -0.3) +
    #  opts(title = "Range of contribution to PA estimate (red = normal)")

    return(p)
}

#
# Compares the range of contributions that each parameter makes to the output
# of two different GLMs.
#
# Args:
#   prefix1: The title of the data frame for the first GLM.
#   data1:   The data frame for the first GLM.
#   glm1:    The first GLM.
#   prefix2: The title of the data frame for the second GLM.
#   data2:   The data frame for the second GLM.
#   glm2:    The second GLM.
#   output:  The name of the output variable predicted by both GLMs.
#   debug:   Whether to print some debugging information.
#
# Returns:
#   The plot object.
#
CompareGLMRanges <- function(prefix1, data1, glm1, prefix2, data2, glm2, output, debug=FALSE) {
  plot.title <- output
  output <- paste("v_", output, sep="")

  #load(file1)
  pfx1.coeffs <- glm1$coefficients[-1]
  pfx1.obs <- data1[names(pfx1.coeffs)]
  pfx1.sdev <- sd(pfx1.obs)
  pfx1.range <- pfx1.coeffs * 4 * pfx1.sdev
  # TODO -- normalise range by the RANGE in the OUTPUT variable
  #pfx1.output <- result$glm.all.Xs$fitted.values + result$glm.all.Xs$residuals
  #pfx1.output.range <- max(pfx1.output) - min(pfx1.output)
  # TODO -- normalise range by the RANGE in the FITTED OUTPUT
  pfx1.output.range <- max(glm1$fitted.values) - min(glm1$fitted.values)
  if (pfx1.output.range == 0) {
    pfx1.output.range <- 1
  }
  pfx1.range <- pfx1.range / pfx1.output.range

  #load(file2)
  pfx2.coeffs <- glm2$coefficients[-1]
  pfx2.obs <- data2[names(pfx2.coeffs)]
  pfx2.sdev <- sd(pfx2.obs)
  pfx2.range <- pfx2.coeffs * 4 * pfx2.sdev
  # TODO -- normalise range by the RANGE in the OUTPUT variable
  #pfx2.output <- result$glm.all.Xs$fitted.values + result$glm.all.Xs$residuals
  #pfx2.output.range <- max(pfx2.output) - min(pfx2.output)
  # TODO -- normalise range by the RANGE in the FITTED OUTPUT
  pfx2.output.range <- max(glm2$fitted.values) - min(glm2$fitted.values)
  if (pfx2.output.range == 0) {
    pfx2.output.range <- 1
  }
  pfx2.range <- pfx2.range / pfx2.output.range

  if (debug) {
    print(pfx1.output.range)
    print(pfx2.output.range)
    print(length(pfx1.range))
    print(length(pfx2.range))
  }

  p <- qplot(x=pfx1.range, y=pfx2.range, label=names(pfx1.coeffs),
             colour=rep("a", length(pfx1.range)), main=plot.title) +
       geom_text(size=3, hjust = -0.3) +
       scale_colour_manual(value=c("#3f3f3f", "#9FC59F")) +
       geom_abline(intercept=0, slope=1, colour="#9FC59F") +
       scale_x_continuous(prefix1, expand=c(0.05, 0.05)) +
       scale_y_continuous(prefix2) +
       opts(legend.position="none")

  return(p)
}

#
# Compares GLMs between two distinct populations, for a range of outputs.
#
# Args:
#   outputs: The list of outputs to which GLMs have been fitted.
#   popn1:   A list containing the details of the first population.
#   popn2:   A list containing the details of the second population.
#
# Returns:
#   A nested list of plot objects, tagged by output name.
#
CompareGLMs <- function(outputs, popn1, popn2) {
  ps = list()

  for (param in names(outputs)) {
    if (outputs[[param]]$type == "gaussian") {
      plot.title <- sprintf("%s (%s)", outputs[[param]]$desc, param)

      file1 <- DataFileName(popn1$prefix, param)
      file2 <- DataFileName(popn2$prefix, param)

      load(file1)
      glm1 <- result$glm.all.Xs
      load(file2)
      glm2 <- result$glm.all.Xs

      rm(result)
      gc()

      p1 <- CompareGLMRanges(popn1$name, popn1$data, glm1,
                             popn2$name, popn2$data, glm2,
                             output=param)
      p2 <- CompareGLMContributions(glm1, popn1$data, glm2, popn2$data,
                                    y.title=output)

      ps[[name]] = list(ranges=p1, contribs=p2)
    }
  }

  return(ps)
}
