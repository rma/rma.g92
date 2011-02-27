#
# Creates a data frame from which heatmaps can be plotted.
#
# Args:
#   data:      The data set of points to be converted into a heatmap.
#   xname:     The name of the x-axis variable in the data set.
#   yname:     The name of the y-axis variable in the data set.
#   fit.fn:    An (optional) fitting function that will also be included.
#   intervals: The number of blocks into which both axes will be divided.
#
# Returns:
#   A data frame for plotting heatmaps.
#
MakeHeatmapFrame <- function(data, xname, yname, fit.fn=NULL, intervals=125) {
  xmin <- min(data[[xname]]) * 0.95
  xmax <- max(data[[xname]]) * 1.05
  ymin <- min(data[[yname]]) * 0.95
  ymax <- max(data[[yname]]) * 1.05

  xs <- seq(from=xmin, to=xmax, length=intervals + 1)
  ys <- seq(from=ymin, to=ymax, length=intervals + 1)
  xs.width <- xs[2] - xs[1]
  ys.width <- ys[2] - ys[1]

  xis <- 1:length(xs)-1
  yis <- 1:length(ys)-1

  X <- c()
  Y <- c()
  for (x in xs[xis]) {
    for (y in ys[yis]) {
      X <- append(X, x)
      Y <- append(Y, y)
    }
  }

  COUNT <- c()
  for (i in 1:length(X)) {
    xx <- X[i]
    yy <- Y[i]
    data.xgt <- data[[xname]] >= xx
    data.xlt <- data[[xname]] < (xx + xs.width)
    data.ygt <- data[[yname]] >= yy
    data.ylt <- data[[yname]] < (yy + ys.width)
    count <- length(data[data.xgt & data.xlt & data.ygt & data.ylt,1])
    COUNT <- append(COUNT, count)
  }

  if (! is.null(fit.fn)) {
    FIT <- fit.fn(X)
    return(data.frame(X, Y, COUNT, FIT))
  } else {
    return(data.frame(X, Y, COUNT))
  }
}

#
# Plots a heatmap.
#
# Args:
#   heat.frame:  A data frame containing the heatmap data.
#   fit.fn:      An (optional) function for plotting a fit to the data.
#   heat.colour: The colour used to display heat intensity.
#   fit.colour:  The colour used to display the fit function.
#
# Returns:
#   The plot object.
#
PlotHeatmap <- function(heat.frame, fit.fn=NULL, heat.colour="steelblue",
                        fit.colour="#cc9393") {
  p <- ggplot(heat.frame) +
       geom_tile(aes(x=X, y=Y, alpha=COUNT), fill=heat.colour) +
       scale_alpha(name="Frequency", to=c(0,1), trans="sqrt")
  if (! is.null(fit.fn)) {
    p <- p + geom_line(aes(x=X, y=FIT), colour=fit.colour, size=1)
  }

  return(p)
}

#
# An example of using MakeHeatmapFrame() to plot heatmaps.
#
# Args:
#   data:  The data frame from which the heatmap will be plotted.
#   col.x: The name or index of the x-axis column (default = 1).
#   col.y: The name or index of the y-axis column (default = 2).
#
# Returns:
#   A list of three heatmap plots.
#
# Example:
#   data(qalh.mdflw)
#   plots <- ExampleHeatmapsMDFLW(qalh.mdflw)
#   # Set the plot width for the call to SavePlots().
#   plots$width <- 9
#   do.call(SavePlots, plots)
#
ExampleHeatmapsMDFLW <- function(data, col.x=1, col.y=2) {
  # discard the first 75 rows
  data <- data[-75:-1,1:2]

  fit.fn <- function(x) {
    # Cubic GLM:
    # return(- 3374.8165 + 1978.1256 * x - 386.4192 * x^2 + 25.1648 * x^3)
    # Linear GLM:
    # return(- 0.3750828 + 0.2689998 * x)
    # Normalisation by mean:
    return(x / 5.077413)
  }

  frame <- MakeHeatmapFrame(data, col.x, col.y, fit.fn, intervals=125)

  x.label <- "Qalh (nL/min) [Moore94]"
  y.label <- "MDFLW [Guyton92]"
  coords <- coord_cartesian(xlim=c(5.05, 5.182), ylim=c(0.98, 1.0205))
  theme1.opts <- opts(plot.background=theme_rect(fill=NA, size=0),
    panel.background = theme_blank(),
    panel.grid.major = theme_line(size = 0.5, colour = "#dcdccc"),
    panel.grid.minor = theme_blank(),
    axis.title.x = theme_text(size = 14, vjust = -0.5, colour = "#dcdccc", face="bold"),
    axis.title.y = theme_text(size = 14, hjust = 0.0, angle = 90, colour = "#dcdccc", face="bold"),
    axis.text.x = theme_text(size = 14, vjust = 1, colour = "#dcdccc"),
    axis.text.y = theme_text(size = 14, hjust = 1, colour = "#dcdccc"),
    axis.ticks = theme_segment(size = 0.5, colour = "#dcdccc"),
    legend.background = theme_rect(fill=NA, colour = "#dcdccc"),
    legend.text = theme_text(size = 14, colour = "#dcdccc"),
    legend.title = theme_text(size = 12, colour = "#dcdccc", face="bold", hjust=0)
  )
  theme2.opts <- opts(plot.background=theme_rect(fill=NA, size=0),
    panel.background = theme_blank(),
    panel.grid.major = theme_line(size = 0.5, colour = "#9f9f9f"),
    panel.grid.minor = theme_blank(),
    axis.title.x = theme_text(size = 14, vjust = -0.5, colour = "#3f3f3f"),
    axis.title.y = theme_text(size = 14, hjust = 0.0, angle = 90, colour = "#3f3f3f"),
    axis.text.x = theme_text(size = 14, vjust = 1, colour = "#3f3f3f"),
    axis.text.y = theme_text(size = 14, hjust = 1, colour = "#3f3f3f"),
    axis.ticks = theme_segment(size = 0.5, colour = "#9f9f9f"),
    legend.background = theme_rect(fill=NA, colour = "#3f3f3f"),
    legend.text = theme_text(size = 14, colour = "#3f3f3f"),
    legend.title = theme_text(size = 12, colour = "#3f3f3f", face="bold", hjust=0)
  )

  p <- PlotHeatmap(frame, fit.fn) +
       xlab(x.label) + ylab(y.label) + coords + theme1.opts

  p <- PlotHeatmap(frame) +
       xlab(x.label) + ylab(y.label) + coords + theme1.opts

  r <- PlotHeatmap(frame, fit.fn) +
       xlab(x.label) + ylab(y.label) + coords + theme2.opts

  return(list(p,q,r))
}
