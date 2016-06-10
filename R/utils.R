#' Simulate coefficients from a GLM by making draws from the multivariate
#' normal distribution
#'
#' @param obj a fitted model object.
#' @param nsim number of simulations to draw
#'
#' @return A data frame of simulated coefficients from \code{obj}.
#'
#' @examples
#' library(car)
#'
#' # Estimate model
#' m1 <- lm(prestige ~ education + type, data = Prestige)
#'
#' # Create fitted values
#' prestige_sims <- b_sim(m1)
#'
#' @importFrom stats coef vcov
#' @importFrom MASS mvrnorm
#'
#' @export

b_sim <- function(obj, nsim = 1000) {
    obj_coef <- coef(obj)
    obj_vcov <- vcov(obj)

    drawn <- mvrnorm(n = nsim, mu = obj_coef, Sigma = obj_vcov)
    drawn <- data.frame(drawn)

    if (grepl('intercept', names(drawn[1]), ignore.case = TRUE))
        names(drawn)[1] <- 'intercept_'
    return(drawn)
}

#' Find the systematic component in the linear form for fitted values in across
#' each simulation (note largely for internal use in \code{\link{qi_builder}})
#'
#' @param b_sims a data frame created by \code{\link{b_sim}} of simulated
#' coefficients
#' @param newdata a data frame of fitted values with column names corresponding
#' to variable names in \code{b_sims}. Variables in \code{b_sim} not present
#' in \code{newdata} will be treated as fitted at 0.
#' @param inc_intercept logical whether to include the intercept in the
#' lineary systematic component.
#'
#' @return A data frame fitted values supplied in \code{newdata} and associated
#' linear systematic component estimates for all simulationed coefficient
#' estimates. The linear systematic components are included in a column
#' named \code{ls_}.
#'
#' @examples
#' library(car)
#'
#' # Estimate model
#' m1 <- lm(prestige ~ education + type, data = Prestige)
#'
#' # Create fitted values
#' fitted_df <- expand.grid(education = 6:16, typewc = 1)
#'
#' # Simulate coefficients
#' m1_sims <- b_sim(m1, nsim = 1000)
#'
#' # Find linear systematic component for fitted values
#' ls <- linear_systematic(b_sims = m1_sims, newdata = fitted_df)
#'
#' @source King, Gary, Michael Tomz, and Jason Wittenberg. 2000. "Making the
#' Most of Statistical Analyses: Improving Interpretation and Presentation."
#' American Journal of Political Science 44(2): 341-55.
#'
#' @export

linear_systematic <- function(b_sims, newdata, inc_intercept = TRUE) {
    fitted_names <- names(newdata)

    if (!('intercept_' %in% fitted_names)) inc_intercept <- FALSE

    if (!all(fitted_names %in% names(b_sims)))
        stop('Unable to find all of the variables from newdata in b_sims.',
            call. = FALSE)

    if (!all(sapply(newdata, class) %in% c('numeric', 'integer')))
        stop('All fitted values must be either numeric or integer.',
            call. = FALSE)

    intercept <- b_sims[['intercept_']]
    not_fitted_0 <- data.matrix(b_sims[, fitted_names])
    sims_fitted <- list(not_fitted_0, data.matrix(newdata))

    if (inc_intercept) {
        ls <- lapply(1:nrow(sims_fitted[[2]]), function(x) {
            fitted_1 <- sims_fitted[[2]][x, ]
            data.frame(
                data.frame(
                    t(fitted_1)),
                    ls_ = intercept + (sims_fitted[[1]] %*% fitted_1)
            )
        })
    }
    else {
        ls <- lapply(1:nrow(sims_fitted[[2]]), function(x) {
            fitted_1 <- sims_fitted[[2]][x, ]
            data.frame(
                data.frame(
                    t(fitted_1)),
                    ls_ = (sims_fitted[[1]] %*% fitted_1)
            )
        })
    }

    ls <- do.call(rbind, ls)
    return(ls)
}

#' Find quantities of interest from generalized linear models
#'
#' @param b_sims a data frame created by \code{\link{b_sim}} of simulated
#' coefficients
#' @param newdata a data frame of fitted values with column names corresponding
#' to variable names in \code{b_sims}. If \code{missing} then a normal
#' linear regression model is assumed and the predicted values are returned
#' (e.g. the fitted linear systematic component from
#' \code{\link{linear_systematic}}).
#' @param model a function for calculating how to find the quantity of interest
#' from a vector of the fitted linear systematic component.
#' @param ci the proportion of the central interval of the simulations to
#' return. Must be in [0, 1].
#' @param ... arguments to pass to \code{\link{linear_systematic}}
#'
#' @return A data frame fitted values supplied in \code{newdata} and associated
#' simulated quantities of interest. The quantities of interest are in a column
#' named \code{qi_}.
#'
#' @examples
#' library(car)
#'
#' # Estimate model
#' m1 <- lm(prestige ~ education + type, data = Prestige)
#' # Simulate coefficients
#' m1_sims <- b_sim(m1)
#'
#' # Create fitted values
#' fitted_df_1 <- expand.grid(education = 6:16, typewc = 1)
#'
#' linear_qi <- qi_builder(b_sims = m1_sims, newdata = fitted_df_1)
#'
#' @importFrom stats quantile
#'
#' @export

qi_builder <- function(b_sims, newdata, model, ci = 0.95, ...) {
    qi_ <- NULL
    if (ci <= 0 | ci > 1) {
        stop("ci must be greater than 0 and not greater than 1.",
             call. = FALSE)
    }

    qi_df <- linear_systematic(b_sims = b_sims, newdata = newdata, ...)

    if (missing(model)) {
        message('Note: model argument missing -> assuming normal linear model.\n')
        names(qi_df)[grep('ls_', names(qi_df))] <- 'qi_'
    }
    else {
        if (!is.function(model)) stop('model must be a function.',
            call. = FALSE)
        qi_df[, 'qi_'] <- model(qi_df[['ls_']])
        qi_df['ls_'] <- NULL
    }

    if (ci < 1) {
        lower <- (1 - ci)/2
        upper <- 1 - lower
        qi_df$scenario_ <- apply(qi_df[, 1:(ncol(qi_df)-1)], 1, paste,
                                collapse = '_')
        qi_list <- split(qi_df, qi_df[['scenario_']])

        qi_list <- lapply(1:length(qi_list), function(x){
            lower_bound <- quantile(qi_list[[x]][,'qi_'], prob = lower)
            upper_bound <- quantile(qi_list[[x]][,'qi_'], prob = upper)
            subset(qi_list[[x]], qi_ >= lower_bound & qi_ <= upper_bound)
        })
        qi_df <- do.call(rbind, qi_list)
        qi_df$scenario_ <- NULL
    }
    return(qi_df)
}
