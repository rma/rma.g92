#
# Stagger tick labels with a prefix or postfix, so that the labels remain
# legible when there are many.
#
# Args:
#   labels:  The names of the tick labels.
#   levels:  The number of levels into which the labels will be staggered.
#   reqd:    A string that is added to every label, regardless of level.
#   add:     The string that is added for each level of staggering.
#   postfix: Whether the staggering should be inserted after (default) or
#            before the label text.
#
# Returns:
#   The list of staggered label names, in the same order as the original list.
#   Note that StaggerLabels() assumes that the labels will be displayed
#   alphabetically and staggers the labels accordingly.
#
StaggerLabels <- function(labels, levels=4, reqd="--", add="------------",
                          postfix=TRUE) {
  #
  # Add some text to a tick label, respecting the value of "postix".
  #
  # Args:
  #   s:     The tick label.
  #   extra: The texxt to be added to the label.
  #
  # Returns:
  #   The modified tick label.
  #
  strcat <- function(s, extra) {
    if (postfix) {
      return(paste(s, extra, sep=""))
    } else {
      return(paste(extra, s, sep=""))
    }
  }

  sorted.inds <- sort(labels, index.return=TRUE)
  for (level in 1:levels) {
    for (i in seq(from=level, to=length(sorted.inds$ix), by=levels)) {
      labels[sorted.inds$ix[i]] <- strcat(labels[sorted.inds$ix[i]], reqd)
      if (level > 1) {
          for (j in 2:level) {
              labels[sorted.inds$ix[i]] <- strcat(labels[sorted.inds$ix[i]], add)
          }
      }
    }
  }
  return(labels)
}

#
# Calculates the mean and standard deviation of all values in a data frame
# whose names begin with a specific prefix.
#
# Args:
#   data:   The data frame.
#   prefix: The prefix common to all of the names that will be examined.
#
# Returns:
#   A nested list of means (mean) and standard deviations (sd), as well as
#   minimum (min) and maximum (max) values, tagged by value name.
#
# Example:
#   data(smallppn)
#   param.dists <- CalculateValueDistributions(smallppn, "p_")
#   cpr.mean <- param.dists$p_CPR$mean
#   cpr.sd <- param.dists$p_CPR$sd
#
CalculateValueDistributions <- function(data, prefix) {
  names <- grep(prefix, names(data), value=TRUE);
  result <- lapply(names, function(n) {
    return(list(mean=mean(data[[n]]), sd=sd(data[[n]]),
                min=min(data[[n]]), max=max(data[[n]])
    ));
  });
  names(result) <- names;
  return(result);
}

#
# Plots value distributions, comparing the distributions in two distinct
# populations to a control population against which the distributions are
# normalised.
#
# Args:
#   control:        The control population.
#   d1:             The first population.
#   l1:             A descriptive name for the first population.
#   d2:             The second population.
#   l2:             A descriptive name for the second population.
#   sd.wrt.sd:      Whether standard deviations are normalised with respect to
#                   the deviations in the control population (default) or the
#                   means in the control population.
#   stagger.labels: Whether the x-axis labels should be rotated and staggered.
#   plot.title:     The title of the plot.
#   min.diff:       The minimum difference in (normalised) mean for a value to
#                   be included in the plot.
#   max.diff:       The maximum difference in (normalised) mean for a value to
#                   be included in the plot.
#   y.lim:          The range of the y-axis.
#
# Returns:
#   A list containing the plot object (plot), the names of the plotted values
#   (names) and the names of the values that exceeded max.diff (too.large).
#
PlotDistsComparison <- function(control, d1, l1, d2, l2, sd.wrt.sd=TRUE,
    stagger.labels=FALSE, plot.title=NULL, min.diff=0.10, max.diff=10.0,
    y.lim=NULL) {

  # Extract the means and deviations for each population.
  control.means <- as.double(map(GetField("mean"), control))
  control.sds <- as.double(map(GetField("sd"), control))
  d1.means <- as.double(map(GetField("mean"), d1))
  d1.sds <- as.double(map(GetField("sd"), d1))
  d2.means <- as.double(map(GetField("mean"), d2))
  d2.sds <- as.double(map(GetField("sd"), d2))

  # Normalise the means and deviations.
  d1.means <- d1.means / control.means
  d2.means <- d2.means / control.means
  if (sd.wrt.sd) {
      d1.sds <- d1.sds / control.sds
      d2.sds <- d2.sds / control.sds
  } else {
      d1.sds <- d1.sds / control.means
      d2.sds <- d2.sds / control.means
  }

  # Remove NaNs from the means and deviations.
  remove.inds <- which(is.nan(d1.means), arr.ind=TRUE)
  remove.inds <- c(remove.inds, which(is.nan(d2.means), arr.ind=TRUE))
  remove.inds <- c(remove.inds, which(is.nan(d1.sds), arr.ind=TRUE))
  remove.inds <- c(remove.inds, which(is.nan(d2.sds), arr.ind=TRUE))
  # Remove means that are too large or too similar to the control.
  remove.inds <- c(remove.inds, which(abs(d1.means) > max.diff, arr.ind=TRUE))
  remove.inds <- c(remove.inds, which(abs(d2.means) > max.diff, arr.ind=TRUE))
  remove.inds <- c(remove.inds, which(abs(d1.means) > (1 - min.diff) & abs(d1.means) < (1 + min.diff) & abs(d2.means) > (1 - min.diff) & abs(d2.means) < (1 + min.diff), arr.ind=TRUE))
  remove.inds <- unique(remove.inds)
  # Record which variables/parameters were too large.
  large.inds <- c(which(abs(d1.means) > max.diff, arr.ind=TRUE),
                  which(abs(d2.means) > max.diff, arr.ind=TRUE))
  names.large <- names(control)[unique(large.inds)]

  # Actually remove the invalid values from the data.
  d1.means <- d1.means[-remove.inds]
  d2.means <- d2.means[-remove.inds]
  d1.sds <- d1.sds[-remove.inds]
  d2.sds <- d2.sds[-remove.inds]
  names.kept <- names(control)[-remove.inds]

  # Build the lists of all means and deviations.
  means = c(d1.means, d2.means)
  sds = c(d1.sds, d2.sds)

  # Generate the labels for the x-axis.
  names.disp <- as.character(map(function(s) { return(substring(s, 3)) }, names.kept))
  if (stagger.labels) {
    names.disp <- StaggerLabels(names.disp)
  }
  print(names.disp)
  xs <- rep(names.disp, 2)

  # Calculate the size of the boxes.
  ymin = means - sds
  ymax = means + sds

  # Categorise the distributions by population.
  name.count <- length(names.kept)
  popn.labels <- c(rep(l1, name.count), rep(l2, name.count))

  # Collect all of the data into a single frame.
  info <- data.frame(
    Population = popn.labels,
    Xs = xs,
    Value = means,
    ymin = ymin,
    ymax = ymax
  )

  # Produce a plot object that encapsulates the data to be plotted.
  p <- ggplot(data=info, aes(x=Xs, y=Value, ymin=ymin, ymax=ymax, colour=Population)) +
       geom_crossbar(width=0.75) + #coord_cartesian(ylim=c(0,6))
       #scale_x_discrete(x.label) +
       opts(axis.text.x=theme_text(size=8, angle=90, hjust=1))
  # Optionally set the plot title.
  if (! is.null(plot.title)) {
    p <- p + opts(title = plot.title)
  }
  # Optionally set the limits on the Y-axis.
  if (! is.null(y.lim)) {
    p <- p + ylim(y.lim)
  }

  # Return the produced plot object.
  return(list(plot=p, names=names.kept, too.large=names.large))
}

#
# A demonstration of how to use PlotDistsComparison().
#
# Args:
#   None.
#
# Returns:
#   Nothing; two plots are printed to the active device.
#
PlotDistsComparisonDemo <- function() {
  p1 <- PlotDistsComparison(var.dists.nt, var.dists.pht, "Pre-hypertensive",
          var.dists.ht, "Hypertensive", max.diff=100)$plot
  print(p1)

  p2 <- PlotDistsComparison(var.dists.nt, var.dists.pht, "Pre-hypertensive",
          var.dists.ht, "Hypertensive", sd.wrt.sd=FALSE,
          min.diff=0.05, max.diff=10, y.lim=c(-5,5))$plot +
        scale_x_discrete("Variable")
  print(p2)
}
