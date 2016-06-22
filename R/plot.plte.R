#' Plot objects created by plte_builder
#'
#' @param obj a \code{plte} class object created by \code{\link{plte_builder}}.
#' @param t_labels logical whether or not to include time labels for each point.
#' @param difference logical whether or not to plot the difference between the
#' counterfactual and baseline probabilities.
#'
#' @examples
#' data('negative')
#'
#' # BTSCS set the data
#' neg_set <- btscs(df = negative, event = 'y', t_var = 'tim',
#'                  cs_unit = 'group', pad_ts = FALSE)
#'                  # Create temporal dependence variables
#' neg_set$t <- neg_set$spell + 1
#'
#' m1 <- glm(y ~ x + t + I(t^2) + I(t^3),
#'           family = binomial(link = 'logit'),
#'           data = neg_set)
#'
#' counterfactual <- data.frame(x = 0.5)
#'
#' sim1 <- plte_builder(obj = m1, obj_tvar = 't',
#'                      cf = counterfactual, t_points = c(13, 25),
#'                      cf_duration = 3, ci = 99)
#'
#' plte_plot(sim1)
#'
#'
#' @import ggplot2
#'
#' @export

plte_plot <- function(obj, t_labels = TRUE, difference = FALSE)
{
    scenario_name <- scenario_time <- qi_median <- qi_min <- qi_max <- t__ <- NULL

    if (!inherits(obj, 'plte'))
        stop('obj must be plte class created by plte_builder.', call. = FALSE)

    ot <- attr(obj, 'obj_tvar')
    names(obj)[names(obj) == ot] <- 't__'
    obj[1, 't__'] <- ''

    sims <- subset(obj, !(scenario_name == 'counterfactual' & scenario_time < 3))

    sims$scenario_name <- factor(sims$scenario_name,
                                 labels = c('Y = 0', 'Y = 1'))

    p <- ggplot(sims, aes(x = scenario_time, y = qi_median,
                          group = scenario_name)) +
        geom_pointrange(aes(ymin = qi_min, ymax = qi_max,
                            linetype = scenario_name),
                        position = position_dodge(width = 0.3)) +
        scale_linetype_discrete(name = 'Counterfactual') +
        scale_x_continuous(breaks = 1:2, labels = c('Baseline', 't')) +
        xlab('') +
        theme_bw()

    if (t_labels)
        p <- p + geom_text(aes(label = t__), alpha = 0.5, nudge_x = 0.3)

    if (!isTRUE(difference)) {
        p <- p + ylab('Pr(Y = 1)\n')
    }
    p
}