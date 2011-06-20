#
# This file contains a number of small utility functions.
#

##############################################################################
# User Input
##############################################################################

#
# Displays a prompt and collects the user's response. This function is
# identical to "gtools::ask" and is only included here so that the "gtools"
# package is not required.
#
# Args:
#   msg: The message to be displayed to the user.
#
# Returns:
#   The input provided by the user.
#
ask <- function(msg = "Press <RETURN> to continue: ") {
  cat(msg)
  readLines(con = stdin(), n = 1)
}

##############################################################################
# Importing Data
##############################################################################

#
# Imports data from a CSV file and saves the table to an external file.
#
# Args:
#   var_name:    The name of the variable to hold the imported table.
#   input_file:  The CSV file from which to import the table.
#   output_file: The RData file to save the imported table to.
#
ImportCSV <- function(var_name, input_file, output_file) {
    assign(var_name, read.table(input_file, header=TRUE, sep=',',
                                colClasses=c('numeric'), comment.char=''));
    save(list=c(var_name), file=output_file, compress='bzip2');
}

##############################################################################
# Workspace management
##############################################################################

#
# Clears the workspace and performs garbage collection.
#
clear_workspace <- function(verbose=TRUE) {
  rm(list = ls(all = TRUE))
  gc(verbose)
}

#
# An improved listing of objects in the R workspace.
#
# This is based on the version given by Dirk Eddelbuettel on StackOverflow
# (http://stackoverflow.com/questions/1358003/) with modifications from both
# JD Long and Tony Breyal to improve the formatting of object sizes.
#
# A shorthand function, lsos(), is also provided below.
#
.ls.objects <- function (pos = 1, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.size <- napply(names, object.size)
    obj.nicesize <- sapply(obj.size, function(r) prettyNum(r, big.mark = ",") )
    obj.prettysize <- napply(names, function(x) {
                        capture.output(print(object.size(x), units = "auto")) })
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.nicesize, obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "NiceSize", "PrettySize", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
        out <- out[c("Type", "NiceSize", "PrettySize", "Rows", "Columns")]
        names(out) <- c("Type", "NiceSize", "PrettySize", "Rows", "Columns")
    if (head)
        out <- head(out, n)
    out
}

#
# A shorthand function for the most common usecase of .ls.objects().
#
# Args:
#   ...: Arguments that will be passed directly to .ls.objects().
#   n:   The number of objects to list.
#
lsos <- function(..., n=10) {
    .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

##############################################################################
# Functional programming and lists
##############################################################################

#
# A wrapper around lapply() that provides an interface similar to the standard
# "map()" function in functional languages such as Haskell and OCaml.
#
# Args:
#   fn:   The function to apply to each element in "data".
#   data: The data to be mapped. This is generally a list or vector, but
#         any object that can be coerced by "base::as.list" is accepted.
#   conv: A conversion function to be applied to the result, since lapply()
#         always returns a list, regardless of the type of "data". The default
#         is to perform no conversion (conv=NULL).
#
# Returns:
#   The result of applying "fn" to each element in "data", as converted by
#   the conversion function "conv".
#
map <- function(fn, data, conv=NULL) {
  result <- lapply(data, fn)
  if (is.null(conv)) {
    return(result)
  } else {
    return(conv(result))
  }
}

#
# Returns a function that retrieves a tagged field from a list. This function
# is specifically useful in combination with "map()" (see the example, below).
#
# Args:
#   name: The name of the tagged field to retrieve.
#
# Returns:
#   A function that will retrieve the value of the tagged field from a list.
#
# Example:
#   some.list <- list(list(x=1, y=2), list(x=2, y=4), list(x=3, y=6))
#   xs <- map(GetField("x"), some.list)
#   print(as.double(xs) == c(1,2,3))
#
GetField <- function(name) {
  return(function(n) { return(n[[name]])});
}

##############################################################################
# Plotting
##############################################################################

ListToFrame <- function(value.list, tag=NULL, facet=NULL) {
    ns <- character()
    vs <- double()
    for (n in names(value.list)) {
        ns <- c(ns, n)
        vs <- c(vs, value.list[[n]])
    }

    value.frame <- data.frame(name=ns, value=vs, stringsAsFactors=FALSE)
    if (! is.null(tag)) {
        value.frame$tag <- rep(tag, length(ns))
    }
    if (! is.null(facet)) {
        value.frame$facet <- rep(facet, length(ns))
    }

    return(value.frame)
}

ConcatenateFrames <- function(frame.list) {
    ns <- character()
    vs <- double()
    ts <- character()
    fs <- character()

    for (f in frame.list) {
        ns <- c(ns, f[["name"]])
        vs <- c(vs, f[["value"]])
        ts <- c(ts, f[["tag"]])
        fs <- c(fs, f[["facet"]])
    }

    frame <- data.frame(name=ns, value=vs, tag=ts, stringsAsFactors=FALSE)

    if (length(ns) == length(fs)) {
        frame$facet <- fs
    }

    return(frame)
}

PlotFrameOfTaggedValues <- function(frame, xlbl, ylbl, tlbl, desc=FALSE,
                                    min.val=NA) {
    ixs <- order(frame$facet, abs(frame$value))
    if (! is.na(min.val)) {
        ixs <- ixs[abs(frame$value)[ixs] > min.val]
    }
    xs <- frame$name[ixs]
    xs <- factor(xs, levels=unique(xs))
    ys <- frame$value[ixs]
    ts <- frame$tag[ixs]
    fs <- frame$facet[ixs]
    fs <- factor(fs, levels=sort(unique(fs)))

    plot.frame <- data.frame(xs=xs, ys=ys, tags=ts, facets=fs)

    p <- ggplot(plot.frame, aes(x=xs, y=ys, colour=tags, shape=tags)) +
         geom_point() +
         scale_colour_hue(tlbl) + scale_shape(tlbl) +
         xlab(xlbl) + ylab(ylbl) +
         coord_flip()

    if (length(unique(frame$facet)) > 1) {
        p <- p + facet_wrap(~ facets, nrow=1, scales="free", drop=FALSE)
    }

    return(p)
}

#
# Plots a list of numerical values.
#
# Args:
#   vals:   The list of named values.
#   xlbl:   The title of the x-axis.
#   ylbl:   The title of the y-axis.
#   squash: Whether to collapse the x-axis to a single value.
#   desc:   Whether to sort the values in descending or ascending order.
#
# Returns:
#   The plot object.
#
PlotListOfValues <- function(vals, xlbl, ylbl, squash=TRUE, desc=TRUE) {
  if (squash) {
    xs <- rep(1, length(vals))
    ys <- as.double(vals)
    labels <- names(vals)
    p <- qplot(x=xs, y=ys) + geom_text(aes(label=labels, hjust=-0.5)) +
         xlab(xlbl) + ylab(ylbl) +
         opts(axis.text.x=theme_text(colour = NA))
  } else {
    ys <- as.double(vals)
    ixs <- sort(abs(ys), index.return=TRUE, decreasing=desc)$ix
    ys <- ys[ixs]
    xs <- names(vals)[ixs]
    p <- qplot(x=factor(xs, levels=xs), y=ys) + xlab(xlbl) + ylab(ylbl)
  }

  return(p)
}

#
# Saves one or more plots as PDF files.
#
# Args:
#   ...:   One or more plots that can be displayed with the "print" function.
#   width: The width of the PDF plots; the default value is 7 (ie, square).
#
# Returns:
#   Nothing.
#
SavePlots <- function(..., width=7) {
  cairo_pdf(width=width)
  for (p in list(...)) {
    print(p)
  }
  dev.off()
}

#
# Executes a function in the context of the "cairo_pdf()" graphics device.
#
# Args:
#   fn:  The function to be executed. This function must take no parameters.
#   ...: All other arguments are passed to "cairo_pdf()".
#
# Returns:
#   Nothing.
#
# Example:
#   PlotFunction <- function() { plot(c(1,2,3), c(2,4,6)) }
#   WithCairo(PlotFunction, onefile=TRUE, filename="Plots.pdf")
#
WithCairo <- function(fn, ...) {
  cairo_pdf(...)
  fn()
  dev.off()
}

##############################################################################
# Partitioning data
##############################################################################

#
# Divides a data frame into a random subset and the remaining data.
#
# Args:
#   frame: The data frame to divide in two.
#   pcnt:  The size of the random subset, relative to the entire frame.
#
# Returns:
#   A list with two values: "subset" contains the randomly-chosen subset
#   and "rest" contains the remaining data from the original frame.
#
RandomSubset <- function(frame, pcnt = 0.10) {
  if (pcnt < 0 || pcnt > 1) {
    warning(paste("Invalid percentage for rnd.subset:", pcnt))
    return(NULL)
  }

  rows <- 1:nrow(frame)
  count <- floor(nrow(frame) * pcnt)
  chosen <- sample(rows, count, replace = TRUE)
  subsets <- list(subset=frame[chosen,], rest=frame[-chosen,])
  return(subsets)
}

#
# Partitions a data frame based on a threshold value for some field.
#
# Args:
#   data:      The data frame containing the data to be partitioned.
#   field:     The field in the data frame to be compared to the threshold.
#   threshold: The threshold value to determine the partitions.
#   output:    A new field name, that records which rows exceed the threshold.
#
# Returns:
#   A list with three values: "data" contains the original frame with the
#   extra (boolean) field "output", where a value of TRUE indicates that
#   "field" exceeded the threshold; "data.under" contains the subset of the
#   data frame that was strictly under the threshold; and "data.over" contains
#   the subset of the data frame that was equal to or exceeded the threshold.
#
# Example:
#   data(smallppn)
#   parts <- PartitionData(smallppn, "v_PA", 106.6, "v_ISHYPER")
#   parts.nt <- parts$data.under
#   parts.ht <- parts$data.over
#
PartitionData <- function(data, field, threshold, output) {
  data[[output]] <- (data[[field]] >= threshold)
  data.under <- subset(data, data[[output]] == FALSE)
  data.over <- subset(data, data[[output]] == TRUE)

  result = list(data = data, data.under = data.under, data.over = data.over)
  return(result)
}

#
# Returns a function to calculate whether values exceed a threshold.
#
# Args:
#   threshold: The value of the threshold.
#
# Returns:
#   A function that calculates whether or not values exceed the specified
#   threshold. This function returns 1 when a value equals or exceeds the
#   threshold, and returns 0 when the values are within the threshold.
#
# Example:
#   data(smallppn)
#   is.hypertensive <- MakeThreshold(106.6)(smallppn$v_PA)
#
MakeThreshold <- function(threshold) {
  return( function(xs) {
  xs[xs < threshold] <- 0
  xs[xs >= threshold] <-1
  return(xs)} )
}
