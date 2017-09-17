#' Commands
#' @param description What does the script do?
#' @importFrom stringr str_split
#' @export
command <- function(description) {
  cmd <- structure(
    list(
      description = description,
      params = list(),
      script = NULL
    ),
    class = 'command'
  )
  cmd
}

#' Argument
#' @param cmd Command with description, args to parse, and list of params
#' @param name Name of argument(s)
#' @param nargs Number of arguments (-1 for unlimited)
#' @param help Description of argument for help page
argument <- function(cmd, name, default = NULL, nargs = 1, help = "") {
  cmd[[name]] <- structure(
    list(
      default = default,
      nargs = nargs,
      help = help
    ),
    class = "argument"
  )
  cmd
}

#' Option
#'
#' @param cmd Command with description, args to parse, and list of params
#' @param ... Long and short options
#' @param default Default option value if none is given
#' @param type Data type (character, logical, integer, numeric, complex)
#' @param choice Vector of possible values
#' @param is.flag Is this a simple logical flag?
#' @param help Description of option for help page
#' @importFrom stringr str_sub str_length str_replace_all
#' @export
option <- function(cmd, ..., default = NULL, type = NULL, choice = NULL, is.flag = FALSE, help = "") {
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

is.long_opt <- function(x) startsWith(x, "--")
is.short_opt <- function(x) !is.long_opt(x) && startsWith(x, "-")
is.valid_name <- function(x) make.names(x) == x

#' Script
#' @param cmd Command with description, args to parse, and list of params
#' @param fun Function to execute with arguments supplied from the command line
#' @importFrom stringr str_detect str_split
#' @importFrom utils tail
#' @export
script <- function(cmd, fun) {
  stopifnot(class(fun) == 'function')
  cmd$script <- fun
  function(args = commandArgs(trailingOnly = TRUE)) {

  }
}

get_defaults <- function(cmd) {
  lapply(cmd$params, function(param) param$default)
}

parse_args <- function(cmd, args) {
  # call getopt here
  # should return list with args and opts

  # Do some basic parsing here to see if "--help" is called

  # Print help page and exit
  if (str_detect(parse, "--help") || parse == "") {
    # Substitute later with formatted help page
    help_page <- NULL
    for (param in params) {
      help_page <- paste(help_page, paste0(param$long_opt, ": ", param$help), sep = "\n")
    }
    return(function() cat(help_page))
  }

  # parse <- unlist(str_split(parse, " "))
  # parse <- parse[parse != ""]
  defaults <- get_defaults(cmd)
  values <- parse_input(cmd)
  values <- merge_lists(defaults, values)

  # for (param_name in names(params)) {
  #   param <- params[[param_name]]
  #   if (class(param) == "option") {
  #     for (parslet in parse) {
  #       if (startsWith(parslet, param$long_opt) || startsWith(parslet, param$short_opt)) {
  #         if (param$type == "logical") {
  #           values[[param_name]] <- !param$default
  #         }
  #         if (str_detect(parslet, "=")) {
  #           split_parslet <- unlist(str_split(parslet, "="))
  #           param_value <- as.type(tail(split_parslet, 1), param$type)
  #           if (!is.null(param$choice)) {
  #             stopifnot(param_value %in% param$choice)
  #           }
  #           values[[param_name]] <- param_value
  #         }
  #       }
  #     }
  #   }
  # }
  #
  function() do.call(fun, values)
}

merge_lists <- function(...) {
  lists <- list(...)
  master_list <- lists[[1]]
  for (lst in lists[-1]) {
    for (name in names(lst)) {
      master_list[[name]] <- lst[[name]]
    }
  }
  master_list
}

#' Simple function to remove option prefix
#' @param x String beginning with '-' or '--'
#' @importFrom stringr str_sub str_length
remove_prefix <- function(x) {
  if (is.long_opt(x))  return(str_sub(x, 3, str_length(x)))
  if (is.short_opt(x)) return(str_sub(x, 2, str_length(x)))
  x
}

as.type <- function(x, type) {
  if (type == "character") return(as.character(x))
  if (type == "logical")   return(as.logical(x))
  if (type == "integer")   return(as.integer(x))
  if (type == "numeric")   return(as.numeric(x))
  if (type == "complex")   return(as.complex(x))
  x
}
