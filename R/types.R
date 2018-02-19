ATOMIC_DATA_TYPES <- c('logical', 'character', 'numeric',
                       'integer', 'double', 'complex')

#' R Atomic Data type
#' @param type String of desired R atomic data type
atomic <- function(type) {
  stopifnot(type %in% ATOMIC_DATA_TYPES)
  atm <- structure(
    list(
      class = type
    ),
    class = 'atomic'
  )
  atm
}

#' Interval
#' @param lower Lower bound of interval
#' @param upper Upper bound of interval
#' @param exclusive Exclude bounds?
#' @param exclude_lower Exclude lower bound?
#' @param exclude_upper Exclude upper bound?
#' @export
interval <- function(lower, upper, exclusive = FALSE,
                     exclude_lower = FALSE, exclude_upper = FALSE) {
  int <- structure(
    list(
      lower = lower,
      upper = upper,
      exclude_lower = exclusive || exclude_lower,
      exclude_upper = exclusive || exclude_upper
    ),
    class = 'interval'
  )
  int
}

#' Choice
#' @param ... Valid values to choose from
#' @export
choice <- function(...) {
  chc <- structure(
    list(
      choices = unlist(list(...))
    ),
    class = 'choice'
  )
  chc
}
