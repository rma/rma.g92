#
# An example list of outputs.
#
outputs <- list(
  PA = list(type="gaussian", desc="Arterial Pressure", unit="mmHg"),
  NOD = list(type="gaussian", desc="Sodium Excretion", unit="mEq/min"),
  KOD = list(type="gaussian", desc="Potassium Excretion", unit="mEq/min"),
  VUD = list(type="gaussian", desc="Urinary Output", unit="L/min"),
  QLO = list(type="gaussian", desc="Left Ventricular Output", unit="L/min"),
  QRO = list(type="gaussian", desc="Right Ventricular Output", unit="L/min"),
  QAO = list(type="gaussian", desc="Aortic Blood Flow", unit="L/min"))

#
# An example list of populations.
#
# popns <- list(
#   nt = list(prefix="nt", name="Normotensive", data=virtppl.nt),
#   pht = list(prefix="pht", name="Pre-hypertensive", data=virtppl.pht),
#   ht = list(prefix="ht", name="Hypertensive", data=virtppl.ht))

#
# Generates plots exhibiting the suitability of GLMs that have been fitted to
# a number of outputs over several distinct data sets (ie, populations).
#
# Args:
#   popns:   A list of the populations, each of which defines a prefix, a name
#            and the data relevant to the population.
#   outputs: A list of the outputs to which GLMs were fitted, each of which
#            must define the unit of the output.
#
# Returns:
#   Nothing; the plots are generated and then saved in a hierarchy of
#   directories, as defined by DestDirName().
#
PlotAllFittedGLMs <- function(popns, outputs) {
  for (p in popns) {
    for (v in names(outputs)) {
      data_file <- DataFileName(p$prefix, v)
      print(paste("Loading", data_file, "..."))
      load(data_file)

      print("Printing plots ...")
      cairo_pdf()
      axisunit <- outputs[[v]]$unit
      ps <- PlotGLM(result$glm.step.Xs, p$data, axisname=v, axisunit=axisunit)
      print(ps)
      dev.off()

      dest.dir <- DestDirName(p$prefix, v)
      dir.create(dest.dir, recursive=TRUE, mode="0755");
      for (i in c(1,2,3,4,5,6,7)) {
        plot.name <- paste("Rplot00", i, ".pdf", sep="");
        plot.dest <- file.path(dest.dir, plot.name);
        print(paste("Moving", plot.name, "to", plot.dest));
        file.rename(plot.name, plot.dest);
      }
    }
  }
  gc(verbose=FALSE);
}

