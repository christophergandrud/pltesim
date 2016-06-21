#' Create simulations for long-term effects in models with temporal dependence
#'
#' @param obj a fitted model object.
#' @param obj_tvar character string specifying the name of the base time variable
#' in \code{obj}.
#' @param cf a data frame with one row containing the counterfactual.
#' Columns should have names that match variables in \code{obj} and contain
#' fitted values to find quantities of interest for.
#' @param cf_duration a character string or numeric specifying the
#' counterfactual's duration. If \code{'permanent'} then the
#' counterfacutal lasts for the full time span in \code{t_points}. If
#' \code{'one-time'} then the counterfactual only lasts for one period.
#' If \code{cf_duration} is numeric then the number specifies the
#' number of time increments at which the counterfactual resets.
#' @param t_points a numeric vector with a minimum length of 2 and a maximum
#' lentgh of 3. The first and last values should be the time starting and ending
#' points for the simulatinos. The (optional) middle value can specify a point
#' between the first and last time points where a subsequent event occurs.
#' @param FUN a function for finding a quantity of interest from the linear
#' systematic component. See \code{\link{qi_builder}}. The default is a function
#' for finding the predicted probability from a logistic regression model.
#' @param nsim number of simulations to draw.
#' @param ci the proportion of the central interval of the simulations to
#' return. Must be in (0, 1] or equivalently (0, 100].
#'
#' @source
#' Williams, Laron K. 2016. "Long-Term Effects in Models with Temporal
#' Dependence". Political Analysis: 1-20.
#'
#' @export

plte_builder <- function(obj, obj_tvar,
                         cf, cf_duration = 'permanent',
                         t_points, FUN = logistic_prob_FUN,
                         ci = 0.95, nsim = 1000)
{
    # Create scenarios to simulate ---------------------------------------------
    if (missing(obj_tvar))
        stop('obj_tvar must be specified.', call. = FALSE)

    if (length(t_points) < 2)
        stop('Must specify at least two t_range values.', call. = FALSE)
    if (length(t_points) > 3)
        stop('t_points can only include 2 or three time points.', call. = FALSE)

    t_start = t_points[1]
    t_end = t_points[length(t_points)]
    t_no_events <- c(t_start, t_start:t_end)
    t_change <- c(rep(t_start, 2), 0:(t_end - t_start - 1))

    if (length(t_points) == 3) {
        temp <- t_no_events
        for (u in 1:length(temp)) {
            if (t_points[2] <= temp[u]) temp[u] <- temp[u] - t_points[2]
            if (temp[u] < t_change[u]) t_change[u] <- temp[u]
        }
    }

    if (nrow(cf) != 1)
        stop('cf must have only one row.', call. = FALSE)

    # Create baseline scenario
    make_zero <- function(x) x * 0
    baseline <- data.frame(apply(cf, 1, make_zero))
    names(baseline) <- names(cf)

    # Find counterfactuals over time
    if (length(cf_duration) != 1)
        stop('cf_duration can only have one value', call. = FALSE)

    npost_base <- (t_end - t_start) + 1

    if (cf_duration == tolower('permanent')) {
        post_base_cf <- df_repeat(cf, npost_base)
        cf <- rbind(baseline, post_base_cf)
    }
    else if (cf_duration == tolower('one-time')) {
        cf <- rbind(baseline, cf, df_repeat(baseline, npost_base - 1))
    }
    else if (is.numeric(cf_duration)) { # temporary
        cf <- rbind(baseline, df_repeat(cf, cf_duration),
                    df_repeat(baseline, npost_base - cf_duration))
    }

    scenarios <- list(baseline = data.frame(cf, t_no_events),
                      counterfactual = data.frame(cf, t_change))

    # Find quantities of interest ----------------------------------------------
    sims <- data.frame()
    for (i in 1:length(scenarios)) {
        temp <- scenarios[[i]]
        names(temp) <- c(names(temp)[-ncol(temp)], obj_tvar)

        temp_sims <- qi_builder(obj = obj, newdata = temp, FUN = FUN,
                           ci = ci, nsim = nsim, slim = TRUE,
                           original_order = TRUE)
        temp_sims$scenario_name <- names(scenarios)[i]
        temp_sims$scenario_time <- 1:nrow(temp_sims)
        sims <- rbind(sims, temp_sims)
    }

    ncs <- ncol(sims)
    sims <- cbind(sims[, c((ncs-1):ncs)], sims[, c(1:(ncs-2))])

    class(sims) <- c('data.frame', 'pltsesim')

    return(sims)

}


