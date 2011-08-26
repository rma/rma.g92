#
# Plot the significant correlations between parameters and variables across
# two populations, highlighting the differences between the two populations.
#
# Args:
#   data1:      The first population.
#   title1:     A descriptive name for the first population.
#   short1:     A short name for the first population (eg, initials).
#   data2:      The second population.
#   title2:     A descriptive name for the second population.
#   short2:     A short name for the second population (eg, initials).
#   examine:    Iterate over the parameters ("par") or the variables ("var").
#   only.names: Restrict the plots to the named parameters/variables.
#   prompt:     Whether to prompt the user before displaying each plot.
#
# Returns:
#   Nothing; the plots are printed to the active device.
#
PlotCrossCorrelations <- function(data1, title1, short1, data2, title2, short2,
    examine=c("par","var"), only.names=NULL, prompt=FALSE) {
  examine <- match.arg(examine)

  all.names <- names(data1)

  var.names <- grep("v_", all.names, value=TRUE)
  # skip all variables whose names start with "IS"
  # on the basis that they are binomial variables
  var.names <- grep("^v_IS", var.names, value=TRUE, invert=TRUE)
  var.titles <- substring(var.names, 3)

  par.names <- grep("p_", all.names, value=TRUE)
  par.titles <- substring(par.names, 3)

  if (examine == "par") {
    name.list <- par.names
    titles <- var.titles
    cmp.names <- var.names
  } else if (examine == "var") {
    name.list <- var.names
    titles <- par.titles
    cmp.names <- par.names
  } else {
    stop(sprintf("Invalid argument for PlotCrossCorrelations: examine"))
  }

  x.title <- paste(title1, " (", short1, ")", sep="")
  y.title <- paste(title2, " (", short2, ")", sep="")
  legend.title <- paste(short2, " vs ", short1, sep="")

  old.theme <- theme_get()
  theme_set(theme_grey(18))

  for (n in sort(name.list)) {
    name <- substring(n, 3)
    if (! is.null(only.names) && ! any(name == only.names)) {
      print(sprintf("Skipping %s", name))
      next
    }

    p <- PlotCrossCorrelation(data1, data2, n, cmp.names,
                              titles, x.title, y.title, legend.title)

    if (prompt && dev.interactive()) {
      prompt <- sprintf("Press <RETURN> to plot %s: ", title)
      ask(msg=prompt)
    }

    print(p)
  }

  theme_set(old.theme)
}

#
# Plot the significant correlations between parameters/variables and some
# chosen output across two populations, highlighting the differences between
# the two populations.
#
# Note: It is almost certainly more convenient to use PlotCrossCorrelations()
# and specify "only.names" than to use this function directly.
#
# Args:
#   data1:        The first population.
#   data2:        The second population.
#   n:            The name of the value to be correlated against.
#   cmp.names:    The names of the values whose correlations will be plotted.
#   titles:       The titles of the plotted values.
#   x.title:      The title of the x-axis.
#   y.title:      The title of the y-axis.
#   legend.title: The title of the plot legend.
#
# Returns:
#   The plot object.
#
PlotCrossCorrelation <- function(data1, data2, n, cmp.names, titles,
    x.title, y.title, legend.title) {

  if (n == "v_PA") {
    name <- "MAP"
  } else {
    name <- substring(n, 3)
  }
  title <- paste("Cross-correlation with", name)

  cor1 <- as.vector(cor(data1[n], data1[cmp.names], method="spearman"))
  cor2 <- as.vector(cor(data2[n], data2[cmp.names], method="spearman"))

  x.max <- max(cor1, na.rm=TRUE)
  x.min <- min(cor1, na.rm=TRUE)
  y.max <- max(cor2, na.rm=TRUE)
  y.min <- min(cor2, na.rm=TRUE)
  x.buf <- 0.1 * (x.max - x.min)
  y.buf <- 0.1 * (y.max - y.min)
  x.max <- x.max + 2 * x.buf
  x.min <- x.min - 0.5 * x.buf
  y.max <- y.max + y.buf
  y.min <- y.min - y.buf
  x2.max <- ifelse(x.max < y.max, x.max, y.max)
  x2.min <- ifelse(x.min > y.min, x.min, y.min)
  y2.max <- ifelse(y.max < x.max, y.max, x.max)
  y2.min <- ifelse(y.min > x.min, y.min, x.min)

  xs <- c(0, x.max, x.max, x2.max, NA,
          0, x.min, x.min, x2.min, NA,
          0, 0, x2.min, x2.min, NA,
          0, 0, x2.max, x2.max, NA,
          0, x.max, x.max, 0, NA,
          0, x.min, x.min, 0, NA)
  ys <- c(0, 0, y2.max, y2.max, NA,
          0, 0, y2.min, y2.min, NA,
          0, y.min, y.min, y2.min, NA,
          0, y.max, y.max, y2.max, NA,
          0, 0, y.min, y.min, NA,
          0, 0, y.max, y.max, NA)
  diff.len <- length(cor1) - length(xs)
  types = c(rep("Decreased", 4), NA,
            rep("Decreased", 4), NA,
            rep("Increased", 4), NA,
            rep("Increased", 4), NA,
            rep("Negated", 4), NA,
            rep("Negated", 4), NA)
  types = c(rep("Decreased", 5),
            rep("Decreased", 5),
            rep("Increased", 5),
            rep("Increased", 5),
            rep("Change in sign", 5),
            rep("Change in sign", 5),
            rep("Change in sign", diff.len))

  NAs <- rep(NA, diff.len)
  polys = data.frame(xs=c(xs, NAs), ys=c(ys, NAs), Change=types)

  skip = abs(cor1) < 0.1 & abs(cor2) < 0.1
  skip[is.na(skip)] <- TRUE
  cor1[skip] <- NA
  cor2[skip] <- NA

  if (! all(is.na(cor1))) {
    p <- qplot(x=cor1, y=cor2, label=titles, main=title, legend=FALSE) +
      scale_fill_hue(name=legend.title) +
      geom_abline(intercept=0, slope=1, colour="#9FC59F") +
      geom_hline(yintercept=0, colour="#DCA3A3") +
      geom_vline(xintercept=0, colour="#DCA3A3") +
      geom_polygon(aes(x=xs, y=ys, fill=Change), data=polys,
                   size=0, alpha=0.2) +
      geom_text(size=4, hjust = -0.3) +
      geom_point() +
      scale_x_continuous(x.title, expand=c(0, 0)) +
      scale_y_continuous(y.title, expand=c(0, 0)) +
      opts(legend.position = c(0.82,0.19),
        legend.background=theme_rect(fill="white"),
        plot.background=theme_rect(fill=NA, size=0))
  } else {
    cat(sprintf("Skipping %s\n", title))
  }

  gc()
  return(p)
}
