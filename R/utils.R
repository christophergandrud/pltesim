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
#' @return a vector of fitted linear systematic component estimates for all
#' simulations.
#'
#' @examples
#' library(car)
#'
#' #### Linear Model ####
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
#' @seealso King, Gary, Michael Tomz, and Jason Wittenberg. 2000. "Making the
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
            intercept + (sims_fitted[[1]] %*% sims_fitted[[2]][x, ])
        })
    }
    else {
        ls <- lapply(1:nrow(sims_fitted[[2]]), function(x) {
            sims_fitted[[1]] %*% sims_fitted[[2]][x, ]
        })
    }

    ls <- c(do.call(rbind, ls))
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
#' @param ci the
#' @param ... arguments to pass to \code{\link{linear_systematic}}
#'
#' @return A vector of estimated quantities of interest for all simulations.
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
#' @export

qi_builder <- function(b_sims, newdata, model, ...) {
    ls <- linear_systematic(b_sims = b_sims, newdata = newdata, ci = 0.95, ...)

    if (missing(model)) {
        message('Note: model argument missing -> assuming normal linear model.\n')
        return(ls)
    } else {
        if (!is.function(model)) stop('model must be a function.',
            call. = FALSE)
        qi <- model(ls)
        return(qi)
    }
}
