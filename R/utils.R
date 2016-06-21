#' Repeat a data frame n number of times and return as a single data frame
#' @noRd

df_repeat <- function(x, n) {
    do.call('rbind', replicate(n, x, simplify = FALSE))
}
