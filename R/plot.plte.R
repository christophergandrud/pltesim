#' Plot objects created by plte_builder
#'
#' @param obj a \code{plte} class object created by \code{\link{plte_builder}}.
#'
#'
#' @import ggplot2
#'
#' @export

plte_plot <- function(obj, ...)
{
    if (!inherits(obj, 'plte'))
        stop('obj must be plte class created by plte_builder.', call. = FALSE)

    ncs <- ncol(obj)
    sims <- obj[, c(1:2, (ncs-2):ncs)]

    sims <- subset(sims, !(scenario_name == 'counterfactual' & scenario_time < 3))

    sims$scenario_name <- factor(sims$scenario_name)

    p <- ggplot(sims, aes(x = scenario_time, y = qi_median,
                          group = scenario_name)) +
        geom_pointrange(aes(ymin = qi_min, ymax = qi_max,
                            linetype = scenario_name)) +
        theme_bw()

    p
}