#
# Plots the attractor of an output over two different data sets.
#
# Args:
#   output: The name of the output whose attractors will be plotted.
#   data1:  The first data set.
#   label1: A label to identify the first data set.
#   data2:  The second data set.
#   label2: A label to identify the second data set.
#   start:  The index at which to start plotting the attractors.
#
# Example:
#   data(qalh.mdflw)
#   m94 <- qalh.mdflw
#   # MDFLW is calculated from Qalh by the following formula:
#   m94$MDFLW <- m94$Qalh / 5.077413
#   # Compare the attractors for "MDFLW", beginning at t=2min.
#   p <- PlotAttractors("MDFLW", qalh.mdflw, "Guyton92", m94, "Moore94", 121) +
#        scale_colour_hue("Model")
#
PlotAttractors <- function(output, data1, label1, data2, label2, start=1) {
  d1 <- data1[[output]][start:length(data1[[output]])]
  d2 <- data2[[output]][start:length(data2[[output]])]

  d1x <- d1[-length(d1)]
  d1y <- d1[-1]
  d2x <- d2[-length(d2)]
  d2y <- d2[-1]

  colour1 <- rep(label1, length(d1) - 1)
  colour2 <- rep(label2, length(d2) - 1)

  p <- qplot() +
       geom_point(aes(x=d1x, y=d1y, colour=colour1), alpha=0.33) +
       geom_point(aes(x=d2x, y=d2y, colour=colour2), alpha=0.33) +
       scale_x_continuous(output) +
       scale_y_continuous(output)

  return(p)
}
