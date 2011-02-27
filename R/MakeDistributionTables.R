#
# Returns data frames that summarise the distribution of parameters and
# variables from some saved workspace.
#
# Args:
#   par.dists.nt:  The normotensive parameter distributions.
#   par.dists.pht: The pre-hypertensive parameter distributions.
#   par.dists.ht:  The hypertensive parameter distributions.
#   var.dists.nt:  The normotensive variable distributions.
#   var.dists.pht: The pre-hypertensive variable distributions.
#   var.dists.ht:  The hypertensive variable distributions.
#   s.vars:        The list of significant variables (optional).
#   s.params:      The list of significant parameters (optional).
#   descs:         A list of parameter and variable descriptions (optional).
#
# Returns:
#   A list containing the two data frames ("params" and "vars").
#
# Example:
#   data(guyton.descs)
#   data(smallppn)
#   data(smallppn.nt)
#   data(smallppn.pht)
#   data(smallppn.ht)
#
#   par.dists.nt <- CalculateValueDistributions(smallppn.nt, "p_")
#   par.dists.pht <- CalculateValueDistributions(smallppn.pht, "p_")
#   par.dists.ht <- CalculateValueDistributions(smallppn.ht, "p_")
#   var.dists.nt <- CalculateValueDistributions(smallppn.nt, "v_")
#   var.dists.pht <- CalculateValueDistributions(smallppn.pht, "v_")
#   var.dists.ht <- CalculateValueDistributions(smallppn.ht, "v_")
#
#   vs <- PlotDistsComparison(var.dists.nt, var.dists.pht,
#       "Pre-hypertensive", var.dists.ht, "Hypertensive", max.diff=100)$names
#   ps <- PlotDistsComparison(par.dists.nt, par.dists.pht,
#       "Pre-hypertensive", par.dists.ht, "Hypertensive", max.diff=100)$names
#
#   tables <- MakeDistributionTables(par.dists.nt, par.dists.pht, par.dists.ht,
#       var.dists.nt, var.dists.pht, var.dists.ht, vs, ps, guyton.descs)
#
#   write.csv(tables$params, file="param_dists.csv")
#   write.csv(tables$vars, file="var_dists.csv")
#
MakeDistributionTables <- function(par.dists.nt, par.dists.pht, par.dists.ht,
                                   var.dists.nt, var.dists.pht, var.dists.ht,
                                   s.vars=NULL, s.params=NULL, descs=NULL) {
  var.nt.means <- c()
  var.pht.means <- c()
  var.ht.means <- c()
  var.nt.sds <- c()
  var.pht.sds <- c()
  var.ht.sds <- c()
  var.plotted <- c()

  for (n in names(var.dists.nt)) {
    var.nt.means <- c(var.nt.means, var.dists.nt[[n]]$mean)
    var.nt.sds <- c(var.nt.sds, var.dists.nt[[n]]$sd)
    var.pht.means <- c(var.pht.means, var.dists.pht[[n]]$mean)
    var.pht.sds <- c(var.pht.sds, var.dists.pht[[n]]$sd)
    var.ht.means <- c(var.ht.means, var.dists.ht[[n]]$mean)
    var.ht.sds <- c(var.ht.sds, var.dists.ht[[n]]$sd)
    var.plotted <- c(var.plotted, (any(n == s.vars)))
  }

  var.frame <- data.frame(
    Name = names(var.dists.nt),
    NT.mean = var.nt.means,
    NT.sd = var.nt.sds,
    PHT.mean = var.pht.means,
    PHT.sd = var.pht.sds,
    HT.mean = var.ht.means,
    HT.sd = var.ht.sds,
    Plotted = var.plotted
  )

  if (! is.null(descs)) {
    ds = c()
    for (n in names(var.dists.nt)) {
      desc <- descs$V2[which(descs$V1 == substring(n, 3))]
      ds <- c(ds, desc)
    }
    var.frame$Description <- ds
  }

  par.nt.means <- c()
  par.pht.means <- c()
  par.ht.means <- c()
  par.nt.sds <- c()
  par.pht.sds <- c()
  par.ht.sds <- c()
  par.plotted <- c()

  for (n in names(par.dists.nt)) {
    par.nt.means <- c(par.nt.means, par.dists.nt[[n]]$mean)
    par.nt.sds <- c(par.nt.sds, par.dists.nt[[n]]$sd)
    par.pht.means <- c(par.pht.means, par.dists.pht[[n]]$mean)
    par.pht.sds <- c(par.pht.sds, par.dists.pht[[n]]$sd)
    par.ht.means <- c(par.ht.means, par.dists.ht[[n]]$mean)
    par.ht.sds <- c(par.ht.sds, par.dists.ht[[n]]$sd)
    par.plotted <- c(par.plotted, (any(n == s.params)))
  }

  par.frame <- data.frame(
    Name = names(par.dists.nt),
    NT.mean = par.nt.means,
    NT.sd = par.nt.sds,
    PHT.mean = par.pht.means,
    PHT.sd = par.pht.sds,
    HT.mean = par.ht.means,
    HT.sd = par.ht.sds,
    Plotted = par.plotted
  )

  if (! is.null(descs)) {
    ds = c()
    for (n in names(par.dists.nt)) {
      desc <- descs$V2[which(descs$V1 == substring(n, 3))]
      ds <- c(ds, desc)
    }
    par.frame$Description <- ds
  }

  return(list(params=par.frame, vars=var.frame))
}
