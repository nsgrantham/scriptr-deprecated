#' Script
#'
#' @param description What does the script do?
#' @export
script <- function(description) {
  scp <- structure(
    list(
      description = description,
      params = list()
    ),
    class = 'script'
  )
  scp
}

#' Argument
#'
#' @param scp Script object
#' @param name Name of argument
#' @param nargs Number of arguments (Inf for unlimited)
#' @param type String of data type, scriptr::interval, or scriptr::choice
#' @param help Description of argument for help page
#' @export
argument <- function(scp, name, type = 'character', nargs = 1, help = "") {
  stopifnot(class(scp) == 'script')
  stopifnot(nargs == Inf || is_integer(nargs))
  stopifnot(nargs > 0)
  if (sum(c(nargs, unlist(get_nargs(scp))) == Inf) > 1) {
    stop("Only one argument with nargs = Inf is allowed.")
  }
  scp$params[[name]] <- structure(
    list(
      nargs = nargs,
      type = type,
      help = help
    ),
    class = 'argument'
  )
  scp
}

#' Option
#'
#' @param scp Script object
#' @param ... Long and short options
#' @param default Default option value if none is given
#' @param type String of atomic data type, scriptr::interval, or scriptr::choice
#' @param flag Is this a simple logical flag?
#' @param help Description of option for help page
#' @importFrom stringr str_replace_all
#' @export
option <- function(scp, ..., default = NULL, type = NULL, flag = FALSE, help = "") {
  stopifnot(class(scp) == 'script')
  opts <- list(...)
  long_opt <- NULL
  short_opt <- NULL
  for (opt in opts) {
    if (is_long_opt(opt)) {
      long_opt <- opt
    } else if (is_short_opt(opt)) {
      short_opt <- opt
    }
  }
  if (is.null(long_opt)) {
    stop("Long option (a hyphen-separated name with prefix '--') is required.")
  }
  name <- str_replace_all(remove_opt_prefix(long_opt), '-', '_')
  stopifnot(is_valid_name(name))

  if (name %in% names(scp$params)) {
    stop(paste(name, "is already defined."))
  }

  if (flag) {
    type <- 'logical'
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

  scp$params[[name]] <- structure(
    list(
      name = long_opt,
      long_opt = long_opt,
      short_opt = short_opt,
      type = type,
      default = default,
      help = help
    ),
    class = 'option'
  )
  scp
}

#' Command
#'
#' @param scp Script object
#' @param cmd Command function
#' @export
command <- function(scp, cmd) {
  stopifnot(class(scp) == 'script')
  stopifnot(class(cmd) == 'function')
  # If scp is passed to script via method chaining with `%>%`, the function
  # returned by command is unable to access scp when it is called, so save
  # scp to the current environment. (Strange... Am I missing something?)
  scp <- scp
  function(...) {
    args <- c(...)
    if (!length(args)) {
      args <- commandArgs(trailingOnly = TRUE)
    }
    if ('--help' %in% args || identical(args, character(0))) {
      cat(build_help_page(scp))
      return(invisible())
    }
    defaults <- get_defaults(scp)
    values <- parse_args(scp, args)
    do.call(cmd, merge_lists(defaults, values))
    invisible(TRUE)
  }
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
