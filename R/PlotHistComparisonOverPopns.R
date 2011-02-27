#
# Plot histograms that compare the distribution of parameters or variables
# between two distinct populations.
#
# Args:
#   prefix1: The prefix that uniquely identifies the first population.
#   data1:   The first population.
#   prefix2: The prefix that uniquely identifies the first population.
#   data2:   The second population.
#   examine: Whether to compare parameters ("par") or variables ("var").
#   show:    Plot histograms ("hist") or probability densities ("dens").
#
# Returns:
#   Nothing; the plots are printed as they are produced.
#
PlotHistComparisonsOverPopns <- function(prefix1, data1, prefix2, data2,
    examine=c("par","var"), show=c("hist","dens")) {
  examine <- match.arg(examine)
  show <- match.arg(show)

  all.names <- names(data1)

  var.names <- grep("v_", all.names, value=TRUE)
  # skip all variables whose names start with "IS"
  # on the basis that they are binomial variables
  var.names <- grep("^v_IS", var.names, value=TRUE, invert=TRUE)

  par.names <- grep("p_", all.names, value=TRUE)
  par.titles <- substring(par.names, 3)

  if (examine == "par") {
    name.list <- par.names
  } else if (examine == "var") {
    name.list <- var.names
  } else {
    stop(sprintf("Invalid argument for cmp.hists: examine"))
  }

  for (n in sort(name.list)) {
    if (dev.interactive()) {
      prompt <- sprintf("Press <RETURN> to plot %s: ", title)
      ask(msg=prompt)
    }

    p <- PlotHistComparisonOverPopns(prefix1, data1, prefix2, data2, n, show)

    print(p)
  }
}

#
# Plot a histogram that compares the distribution of a parameter or variable
# between two distinct populations.
#
# Args:
#   prefix1: The prefix that uniquely identifies the first population.
#   data1:   The first population.
#   prefix2: The prefix that uniquely identifies the first population.
#   data2:   The second population.
#   n:       The name of the parameter or variable (with leading "p_" or "v_").
#   show:    Plot histograms ("hist") or probability densities ("dens").
#
# Returns:
#   Nothing; the plots are printed as they are produced.
#
PlotHistComparisonOverPopns <- function(prefix1, data1, prefix2, data2, n,
    show=c("hist","dens")) {

  show <- match.arg(show)
  len1 = length(data1[[1]])
  len2 = length(data2[[1]])
  title <- substring(n, 3)

  hist.data <- c(data1[[n]], data2[[n]])
  hist.popn <- c(rep(prefix1, len1), rep(prefix2, len2))
  bincount <- 40
  binwidth <- (max(hist.data) - min(hist.data)) / bincount
  tmp.frame <- data.frame(Data=hist.data, Popn=hist.popn)

  if (show == "hist") {
    p <- qplot(main=title, data=tmp.frame) +
      geom_histogram(aes(x=Data, y=..density.., fill=Popn),
                     alpha=0.3, binwidth=binwidth, position="dodge") +
      xlab("") +
      ylab("") +
      opts(
        legend.background=theme_rect(fill="white"),
        legend.title = theme_blank())
  } else {
    p <- qplot(main=title, data=tmp.frame) +
      geom_density(aes(data1[[n]], fill=prefix1), alpha = 0.33) +
      geom_density(aes(data2[[n]], fill=prefix2), alpha = 0.33) +
      xlab("") +
      ylab("Density") +
      opts(
        legend.background=theme_rect(fill="white"),
        legend.title = theme_blank())
  }
  print(p)
}
