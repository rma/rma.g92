#
# For some parameters, there are one or two missing simulations in the virtual
# population (virtppl). This function is a step towards determining which
# simulations were not included in the results.
#
MissingParams <- function() {
  load("virtppl.RData")

  ixPRE <- seq(from=1, to=227978, by=2)
  ixPOST <- seq(from=2, to=227978, by=2)

  missing.params <- c("p_ADHTC", "p_AMCSNS", "p_VNTSTM", "p_RFABKM", "p_RABSC",
                      "p_ANMAM", "p_AUC1", "p_MDFLWX", "p_AARK", "p_SR2")

  missing.details = list()
  for (p in missing.params) {
    all.values <- virtppl[[p]]
    delta.exps <- which(all.values[ixPRE] != all.values[ixPOST], arr.ind=TRUE)
    missing.details[[p]] = list(pre=all.values[ixPRE][delta.exps],
                                post=all.values[ixPOST][delta.exps])
  }

  rm(virtppl)
  gc()

  save(missing.params, missing.details, file="MissingParams.RData",
       compress="bzip2")
}

