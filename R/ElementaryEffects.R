#
# Calculates elementary effects of parameters on outputs.
#
# Args:
#   experiments: the data frame of delta-perturbation experiments.
#   outliers.rm: whether to remove outliers before calculating statistics.
#   stats.only:  whether to only return the means and deviations.
#   norm:        whether to normalise effects to percentage changes.
#   log:         whether to print diagnostic output.
#
# Returns:
#   A list of the elementary effects (effects), the mean effects (mean) and
#   standard deviation of effects (sd) for each parameter in the data frame.
#
ElementaryEffects <- function(experiments, outliers.rm=TRUE, stats.only=TRUE,
                              norm=TRUE, log=FALSE) {
    exp.count <- dim(experiments)[1]
    names.all <- names(experiments)
    names.par <- grep("p_", names.all, value=TRUE)
    names.var <- grep("v_", names.all, value=TRUE)

    ix.pre <- seq(from=1, to=exp.count, by=2)
    ix.post <- seq(from=2, to=exp.count, by=2)

    effects <- list()
    effect.stats <- list()

    for (p in names.par) {
        # Find which experiments had changes in param p
        exps.pre <- experiments[[p]][ix.pre]
        exps.post <- experiments[[p]][ix.post]
        exps.diff <- exps.post - exps.pre

        delta.exps <- which(exps.diff != 0)
        if (length(delta.exps) == 0) {
            warn.msg <- paste("No perturbations for parameter", p)
            print(warn.msg, quote=FALSE)
            next
        }

        if (log) {
            log.msg <- paste(p, ":", length(delta.exps), "experiments")
            print(log.msg, quote=FALSE)
        }

        vars.pre <- experiments[ix.pre[delta.exps], names.var]
        vars.post <- experiments[ix.post[delta.exps], names.var]

        # Ignore any experiments where the parameter was zero prior to the
        # delta perturbation, as to avoid dividing by zero when normalising.
        vars.post[vars.pre == 0] <- NA
        vars.pre[vars.pre == 0] <- NA

        if (norm) {
            # Normalise the elementary effects.
            p.effects <- (vars.post - vars.pre) / vars.pre
        } else {
            # The formulation given by Morris in Technometrics 33(2):161--174.
            p.effects <- (vars.post - vars.pre) / exps.diff[delta.exps]
        }

        if (outliers.rm) {
            # Remove outliers, based on the "median/MAD method" presented in
            # AMC Technical Brief number 6, April 2001 (ISSN 1757-5958).
            # list of [var.name, row]

            # Provide a median function that removes NA values.
            fn.median <- function(x) { return(median(x, na.rm=TRUE)) }

            # Calculate the median of each elementary effect.
            p.medians <- lapply(p.effects, fn.median)
            # Calculate the deviations from the median.
            p.devns <- p.effects - p.medians
            # Calculate the median of these deviations.
            p.med.devn <- lapply(abs(p.devns), fn.median)
            # Normalise the deviations by the median of the deviations.
            p.med.devn[p.med.devn == 0] <- NA
            p.devn.magn <- abs(p.devns) / p.med.devn

            # Consider deviations more than 2.5 times the median deviation to
            # be outliers, and remove them from the elementary effects table.
            p.outlier.count <- sum(p.devn.magn > 2.5, na.rm=TRUE)
            p.all.count <- prod(dim(p.effects))
            p.outlier.pcnt <- round(100 * p.outlier.count / p.all.count)
            if (log) {
                log.msg <- paste("  Outliers: ", p.outlier.pcnt, "%", sep="")
                print(log.msg, quote=FALSE)
            }
            p.effects[p.devn.magn > 2.5] <- NA
        }

        # Use the built-in mean() and sd() functions, removing NA values.
        effects[[p]] <- list(
            mean=as.list(mean(p.effects, na.rm=TRUE)),
            sd=as.list(sd(p.effects, na.rm=TRUE)))

        if (! stats.only) {
            effects[[p]]$effects <- p.effects
        }
    }

    return(effects)
}

#
# Extracts the elementary effects on a single variable.
#
# Args:
#   effects:  the elementary effects, as calculated by ElementaryEffects().
#   var.name: the name of the variable.
#
# Returns:
#   A list of the mean effects (mean) and standard deviations (sd), both
#   tagged by parameter name.
#
EffectsOnVariable <- function(effects, var.name) {
    effects.on.v <- list(mean=list(), sd=list())

    for (p in grep("p_", names(effects), value=TRUE)) {
        effects.on.v$mean[[p]] <- effects[[p]]$mean[[var.name]]
        effects.on.v$sd[[p]] <- effects[[p]]$sd[[var.name]]
    }

    return(effects.on.v)
}

#
# Plots the elementary effects on a single variable.
#
# Args:
#   effects:  the elementary effects, as calculated by ElementaryEffects().
#   var.name: the name of the variable.
#   min.mean: the minimum (absolute) mean of the plotted effects.
#   min.sd:   the minimum standard deviation of the plotted effects.
#   stagger:  whether to stagger the parameter names on the y-axis.
#
# Returns:
#   The plot object.
#
PlotEffectsOnVariable <- function(effects, var.name, min.mean=NA, min.sd=NA,
    stagger=FALSE) {
    effects.on.v <- EffectsOnVariable(effects, var.name)

    ys <- as.double(effects.on.v$mean)

    # Sort from largest absolute mean to smallest absolute mean.
    ixs <- sort(abs(ys), index.return=TRUE, decreasing=FALSE)$ix
    ys <- ys[ixs]

    short.name <- function(s) { return(substring(s, 3)) }
    xs <- as.character(map(short.name, names(effects.on.v$mean)[ixs]))
    sds <- as.double(effects.on.v$sd[ixs])
    ymins <- ys - sds
    ymaxs <- ys + sds

    # Only plot parameters whose elementary effects exceed the minimums.
    if (! is.na(min.mean) && ! is.na(min.sd)) {
        # Elementary effects need only exceed one of the two minimums.
        ixs <- which(abs(ys) > min.mean | abs(sds) > min.sd)
    } else if (! is.na(min.mean)) {
        ixs <- which(abs(ys) > min.mean)
    } else if (! is.na(min.sd)) {
        ixs <- which(abs(sds) > min.sd)
    } else if (length(ys) > 0) {
        ixs <- 1:length(ys)
    } else {
        len.msg <- paste("WARNING: no VALID values for", var.name)
        return(NULL)
    }

    # Remove infinite and non-numeric values.
    old.length <- length(ixs)
    invalid.means <- which(is.na(ys[ixs]) | is.infinite(ys[ixs]))
    if (length(invalid.means) > 0) {
        ixs <- ixs[- invalid.means]
    }
    invalid.sds <- which(is.na(ys[ixs]) | is.infinite(ys[ixs]))
    if (length(invalid.sds) > 0) {
        ixs <- ixs[- invalid.sds]
    }
    new.length <- length(ixs)

    if (new.length == 0) {
        len.msg <- paste("WARNING: removed ALL values for", var.name)
        print(len.msg, quote=FALSE)
        return(NULL)
    } else if (new.length != old.length) {
        len.msg <- paste("WARNING: removed", old.length - new.length,
                         "values out of", old.length, "for", var.name)
        print(len.msg, quote=FALSE)
    }

    if (stagger) {
        x.lbls <- StaggerLabels(xs[ixs], levels=2, do.sort=FALSE,
                                reqd="", add="                ")
    } else {
        x.lbls <- xs[ixs]
    }

    plot.frame <- data.frame(
        x = x.lbls,
        y = ys[ixs],
        ymin = ymins[ixs],
        ymax = ymaxs[ixs])

    # Label the main axis "Elementary Effect on <var.name> (mu +/- sigma)".
    axis.title <- eval(substitute(expression(
        paste("Elementary Effect on ", v, " (", mu %+-% sigma, ")")),
        list(v=substring(var.name, 3)
        )))

    p <- qplot(data=plot.frame, geom="pointrange",
               x=factor(x, levels=x), y=y, ymin=ymin, ymax=ymax) +
         scale_x_discrete("Parameter") +
         scale_y_continuous(axis.title) +
         coord_flip()

    return(p)
}

#
# Plots the elementary effects on every variable.
#
# Args:
#   effects:  the elementary effects as calculated by ElementaryEffects().
#   min.mean: the minimum (absolute) mean of the plotted effects.
#   min.sd:   the minimum standard deviation of the plotted effects.
#
# Returns:
#   The list of plot objects, tagged by variable name.
#
PlotEffectsOnAllVariables <- function(effects, min.mean=NA, min.sd=NA) {
    plots <- list()
    var.names <- names(effects[[1]]$mean)

    for (v in var.names) {
        plot <- PlotEffectsOnVariable(effects, v, min.mean, min.sd)
        if (! is.null(plot)) {
            plots[[v]] <- plot
        }
    }

    return(plots)
}

#
# Prints elementary effects plots from the smallppn data set.
#
# Args:
#   None.
#
# Returns:
#   The elementary effects, as calculated by ElementaryEffects().
#
PlotEffectsExample <- function() {
    data(smallppn)
    smallppn.ee <- ElementaryEffects(smallppn)
    plots <- PlotEffectsOnAllVariables(smallppn.ee, min.mean=0.1, min.sd=0.2)
    for (p in sort(names(plots))) {
        plot <- plots[[p]]
        if (! is.null(plot)) {
            print(plot)
        }
    }

    return(smallppn.ee)
}

#
# Compares the elementary effects on a single variable for multiple sets of
# elementary effects.
#
# Args:
#   effects:  a list of sets of elementary effects.
#   var.list: a list of variables to inspect, where each variable has a name
#             (name), and a minimum (absolute) mean (min.mean) and deviation
#             (min.sd) that define which effects are significant.
#   stagger:  whether to stagger the parameter names on the y-axis.
#
# Returns:
#   The plot object.
#
CompareEffects <- function(effect.list, var.list, stagger=FALSE) {

    all.xs <- character()
    all.ys <- double()
    all.ymins <- double()
    all.ymaxs <- double()
    all.sets <- character()
    all.vars <- character()

    short.name <- function(s) { return(substring(s, 3)) }

    for (var in var.list) {
    var.name <- var$name
    min.mean <- var$min.mean
    min.sd <- var$min.sd
    if (is.null(min.mean)) {
        min.mean <- NA
    }
    if (is.null(min.sd)) {
        min.sd <- NA
    }

    i <- 1
    for (effects in effect.list) {
        effects.on.v <- EffectsOnVariable(effects, var.name)

        # Sort from largest absolute mean to smallest absolute mean.
        ys <- as.double(effects.on.v$mean)
        ixs <- sort(abs(ys), index.return=TRUE, decreasing=TRUE)$ix
        ys <- ys[ixs]

        xs <- as.character(map(short.name, names(effects.on.v$mean)[ixs]))
        sds <- as.double(effects.on.v$sd[ixs])
        ymins <- ys - sds
        ymaxs <- ys + sds

        # Only plot parameters whose elementary effects exceed the minimums.
        if (! is.na(min.mean) && ! is.na(min.sd)) {
            # Elementary effects need only exceed one of the two minimums.
            ixs <- which(abs(ys) > min.mean | abs(sds) > min.sd)
        } else if (! is.na(min.mean)) {
            ixs <- which(abs(ys) > min.mean)
        } else if (! is.na(min.sd)) {
            ixs <- which(abs(sds) > min.sd)
        } else if (length(ys) > 0) {
            ixs <- 1:length(ys)
        } else {
            len.msg <- paste("WARNING: no VALID values for", var.name)
            return(NULL)
        }

        # Remove infinite and non-numeric values.
        old.length <- length(ixs)
        invalid.means <- which(is.na(ys[ixs]) | is.infinite(ys[ixs]))
        if (length(invalid.means) > 0) {
            ixs <- ixs[- invalid.means]
        }
        invalid.sds <- which(is.na(ys[ixs]) | is.infinite(ys[ixs]))
        if (length(invalid.sds) > 0) {
            ixs <- ixs[- invalid.sds]
        }
        new.length <- length(ixs)

        if (new.length == 0) {
            len.msg <- paste("WARNING: removed ALL values for", var.name)
            print(len.msg, quote=FALSE)
            return(NULL)
        } else if (new.length != old.length) {
            len.msg <- paste("WARNING: removed", old.length - new.length,
                             "values out of", old.length, "for", var.name)
            print(len.msg, quote=FALSE)
        }

        if (stagger) {
            x.lbls <- StaggerLabels(xs[ixs], levels=2, do.sort=FALSE,
                                    reqd="", add="                ")
        } else {
            x.lbls <- xs[ixs]
        }

        if (! is.null(effects$label)) {
            set.name <- effects$label
        } else {
            set.name <- sprintf("Set %d", i)
        }

        all.xs <- c(all.xs, x.lbls)
        all.ys <- c(all.ys, ys[ixs])
        all.ymins <- c(all.ymins, ymins[ixs])
        all.ymaxs <- c(all.ymaxs, ymaxs[ixs])
        all.sets <- c(all.sets, rep(set.name, length(x.lbls)))
        all.vars <- c(all.vars, rep(substring(var.name, 3), length(x.lbls)))

        i <- i + 1
    }

    }

    # Label the main axis "Elementary Effects (mu +/- sigma)".
    axis.title <- eval(substitute(expression(
        paste("Elementary Effects (", mu %+-% sigma, ")")),
        list(v=substring(var.name, 3)
        )))

    lines.position <- position_dodge(width=0.85, height=0)

    plot.frame <- data.frame(
        xs = factor(all.xs, levels=rev(unique(all.xs))),
        ys = all.ys,
        ymin = all.ymins,
        ymax = all.ymaxs,
        sets = factor(all.sets, levels=unique(all.sets)),
        vars = factor(all.vars, levels=unique(all.vars))
        )

    p <- ggplot(data=plot.frame, aes(x=xs, y=ys, ymin=ymin, ymax=ymax,
                colour=sets, shape=sets)) +
         geom_pointrange(position=lines.position, size=0.65) +
         scale_colour_hue("Time") +
         scale_shape("Time") +
         scale_x_discrete("Parameter") +
         scale_y_continuous(axis.title) +
         coord_flip() + facet_wrap(~ vars, nrow=1, scales="free")

    return(p)
}
