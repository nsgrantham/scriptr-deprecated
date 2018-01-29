#' Commands
#' @param description What does the script do?
#' @export
command <- function(description) {
  cmd <- structure(
    list(
      description = description,
      params = list()
    ),
    class = "command"
  )
  cmd
}

#' Argument
#' @param cmd Command with description and list of params
#' @param name Name of argument
#' @param nargs Number of arguments (Inf for unlimited)
#' @param type String of data type, scriptr::interval, or scriptr::choice
#' @param help Description of argument for help page
#' @export
argument <- function(cmd, name, type = "character", nargs = 1, help = "") {
  stopifnot(class(cmd) == "command")
  stopifnot(nargs == Inf || is.wholenumber(nargs))
  stopifnot(nargs > 0)
  if (sum(c(nargs, unlist(get_nargs(cmd))) == Inf) > 1) {
    stop("Only one argument with nargs = Inf is allowed.")
  }
  cmd$params[[name]] <- structure(
    list(
      nargs = nargs,
      type = type,
      help = help
    ),
    class = "argument"
  )
  cmd
}

#' Option
#'
#' @param cmd Command with description and list of params
#' @param ... Long and short options
#' @param default Default option value if none is given
#' @param type String of atomic data type, scriptr::interval, or scriptr::choice
#' @param is.flag Is this a simple logical flag?
#' @param help Description of option for help page
#' @importFrom stringr str_replace_all
#' @export
option <- function(cmd, ..., default = NULL, type = NULL, is.flag = FALSE, help = "") {
  stopifnot(class(cmd) == "command")
  opts <- list(...)
  long_opt <- NULL
  short_opt <- NULL
  for (opt in opts) {
    if (is.long_opt(opt)) {
      long_opt <- opt
    } else if (is.short_opt(opt)) {
      short_opt <- opt
    }
  }
  if (is.null(long_opt)) {
    stop("Long option (a hyphen-separated name with prefix '--') is required.")
  }
  name <- str_replace_all(remove_prefix(long_opt), "-", "_")
  stopifnot(is.valid_name(name))

  if (name %in% names(cmd$params)) {
    stop(paste(name, "is already defined."))
  }

  if (is.flag) {
    type <- "logical"
    default <- if (!is.null(default)) default else FALSE
  }
  if (is.null(type)) {
    if (!is.null(default)) {
      type <- class(default)
    } else {
      stop(paste("Type declaration required for", name, "since no default is given."))
    }
  }
  if (!(class(type) %in% c('choice', 'interval')) &&
      (type %in% ATOMIC_DATA_TYPES)) {
    type <- atomic(type)
  }

  cmd$params[[name]] <- structure(
    list(
      name = long_opt,
      long_opt = long_opt,
      short_opt = short_opt,
      type = type,
      default = default,
      help = help
    ),
    class = "option"
  )
  cmd
}

ATOMIC_DATA_TYPES <- c('logical', 'character', 'numeric', 'integer', 'double', 'complex')

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

#' Script
#' @param cmd Command with description and list of params
#' @param fun Function to execute with arguments supplied from the command line
#' @export
script <- function(cmd, fun) {
  stopifnot(class(cmd) == "command")
  stopifnot(class(fun) == "function")
  # If cmd is passed to script via method chaining with `%>%`, the function
  # returned by script is unable to access cmd when it is called, so save
  # cmd to the current environment. (Strange... Am I missing something?)
  cmd <- cmd
  function(...) {
    args <- c(...)
    if (!length(args)) {
      args <- commandArgs(trailingOnly = TRUE)
    }
    if ("--help" %in% args || identical(args, character(0))) {
      cat(build_help_page(cmd))
      return(invisible())
    }
    defaults <- get_defaults(cmd)
    values <- parse_args(cmd, args)
    do.call(fun, merge_lists(defaults, values))
    invisible(TRUE)
  }
}

#' Interval
#'
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
      exclude_lower = ifelse(exclusive, TRUE, exclude_lower),
      exclude_upper = ifelse(exclusive, TRUE, exclude_upper)
    ),
    class = "interval"
  )
  int
}

#' Choice
#'
#' @param ... Valid values to choose from
#' @export
choice <- function(...) {
  cho <- structure(
    list(
      choices = unlist(list(...))
    ),
    class = "choice"
  )
  cho
}

#' Get default values of params
#'
#' @param cmd Command with description and list of params
get_defaults <- function(cmd) {
  lapply(cmd$params, function(x) x$default)
}

#' Get all argument params
#'
#' @param cmd Command with description and list of params
get_arguments <- function(cmd) {
  Filter(function(x) class(x) == "argument", cmd$params)
}

#' Get all nargs value from arguments
#'
#' @param cmd Command with description and list of params
get_nargs <- function(cmd) {
  lapply(get_arguments(cmd), function(x) x$nargs)
}

#' Get all option params
#'
#' @param cmd Command with description and list of params
get_options <- function(cmd) {
  Filter(function(x) class(x) == "option", cmd$params)
}

#' Pipe
#'
#' scriptr is designed for use with magrittr's pipe function, \code{\%>\%},
#' which turns function composition into a series of imperative statements.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs,rhs A command and an argument, option, or script to apply to it
NULL
