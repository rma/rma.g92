\name{smallppn}
\docType{data}
\alias{smallppn}
\title{A small population of virtual individuals}
\description{
  A randomly-chosen sample of the entire virtual population that was
  generated in order to calculate the elemntary effects of each model
  parameter on each output variable.
}
\usage{data(smallppn)}
\format{A data frame containing 2280 individuals (rows). Each individual
  is a collection of parameters (columns prefixed by "p_") and
  steady-state variables (columns prefixed by "v_"). Descriptions of the
  parameters and variables are stored in "guyton.descs". Each pair of
  rows were obtained from a delta-perturbation simulation; the even rows
  are the result of perturbing a single parameter from the preceeding
  (odd) rows.}
\keyword{datasets}
