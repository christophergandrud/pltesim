#' Plot objects created by plte_builder
#'
#' @param obj a \code{plte} class object created by \code{\link{plte_builder}}.
#' @param difference logical whether or not to plot the difference between the
#' counterfactual and baseline probabilities.
#'
#' @import ggplot2
#'
#' @export

plte_plot <- function(obj, difference = FALSE)
{
    if (!inherits(obj, 'plte'))
        stop('obj must be plte class created by plte_builder.', call. = FALSE)

    names(obj)[names(obj) == attr(obj, 'obj_tvar')] <- 't__'
    sims <- subset(obj, !(scenario_name == 'counterfactual' & scenario_time < 3))

    sims$scenario_name <- factor(sims$scenario_name)

    p <- ggplot(sims, aes(x = scenario_time, y = qi_median,
                          group = scenario_name)) +
        geom_pointrange(aes(ymin = qi_min, ymax = qi_max,
                            linetype = scenario_name),
                        position = position_dodge(width = 0.5)) +
        geom_text(aes(label = t__), hjust = -2, vjust = -1, alpha = 0.5) +
        xlab('') +
        theme_bw()

    if (!isTRUE(difference)) {
        p <- p + ylab('Pr(Y = 1)')
    }
    p
}