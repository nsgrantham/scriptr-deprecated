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
#' @param nargs Number of arguments (-1 for unlimited)
#' @param help Description of argument for help page
#' @export
argument <- function(cmd, name, nargs = 1, help = "") {
  stopifnot(class(cmd) == "command")
  stopifnot(nargs == Inf || is.wholenumber(nargs))
  stopifnot(nargs > 0)
  if (sum(c(nargs, unlist(get_nargs(cmd))) == Inf) > 1) {
    stop("Only one argument with nargs = Inf is allowed.")
  }
  cmd$params[[name]] <- structure(
    list(
      nargs = nargs,
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
#' @param type Data type (character, logical, integer, numeric, complex)
#' @param choice Vector of possible values
#' @param is.flag Is this a simple logical flag?
#' @param help Description of option for help page
#' @importFrom stringr str_replace_all
#' @export
option <- function(cmd, ..., default = NULL, type = NULL, choice = NULL, is.flag = FALSE, help = "") {
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
    default <- if(!is.null(default)) default else FALSE
  }
  if (is.null(type)) {
    if (!is.null(default)) {
      type <- class(default)
    } else {
      stop(paste("Type declaration required for", name, "since no default is given."))
    }
  }

  cmd$params[[name]] <- structure(
    list(
      long_opt = long_opt,
      short_opt = short_opt,
      choice = choice,
      type = type,
      default = default,
      help = help
    ),
    class = "option"
  )
  cmd
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
