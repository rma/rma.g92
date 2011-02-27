#
# Selects a randomly-chosen subset of a population, where every pair of
# adjacent rows store the steady-state achieved before and after a delta
# perturbation.
#
# Args:
#   popn: The population that will be randomly sampled.
#   pcnt: The relative size of the sampled population (default is 1%).
#
# Returns:
#   A list that contains the randomly sampled population (smallppn) and
#   three sub-populations categorised by mean arterial pressure:
#   normotensives (smallppn.nt), pre-hypertensives (smallppn.pht) and
#   hypertensives (smallppn.ht).
#
CreateSmallPopulation <- function(popn, pcnt=0.01) {
  popn.count <- dim(popn)[1]
  popn.pick <- round(popn.count * pcnt / 2)

  chosen <- sample.int(popn.count, popn.pick)

  chosen.post <- 2*chosen
  chosen.pre <- chosen.post - 1
  chosen.ixs <- sort(c(chosen.pre, chosen.post))

  smallppn <- popn[chosen.ixs,]
  smallppn$v_ISHYPER <- smallppn$v_PA >= 106.6

  smallppn.nt <- smallppn[smallppn$v_PA < 93.3,]
  smallppn.pht <- smallppn[smallppn$v_PA >= 93.3 & smallppn$v_PA < 106.6,]
  smallppn.ht <- smallppn[smallppn$v_PA >= 106.6,]

  smallppn.nt$v_ISHYPER <- NULL
  smallppn.pht$v_ISHYPER <- NULL
  smallppn.ht$v_ISHYPER <- NULL

  return(list(smallppn=smallppn, smallppn.nt=smallppn.nt,
              smallppn.pht=smallppn.pht, smallppn.ht=smallppn.ht))
}

