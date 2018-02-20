#' Parse command-line arguments using standard libc functions
#'
#' @param scp Script object
#' @param args Command line args
#' @export
parse_args <- function(scp, args) {
  getopt_params <- lapply(get_options(scp), prepare_getopt_param)
  # HACK: libc in getopt assumes first element of args vector is the command
  # call and skips it, so give it empty string to skip
  getopt_args <- c("", args)
  getopt_values <- callgetopt(args = getopt_args, opts = getopt_params)
  if (!is.null(attr(getopt_values, "error"))) {
    stop("Error parsing command arguments")
  }
  process_getopt_values(scp, getopt_values)
}

#' Translate a single parameter element to the format required for getopt
#'
#' @param param List containing parameter details
#' @export
prepare_getopt_param <- function(param) {
  if (class(param) == "option") {
    if ((class(param$type) == "atomic") && (param$type$class == "logical")) {
      opttype <- "flag"
    } else {
      opttype <- "required"
    }
    name <- if (!is.null(param$long_opt)) remove_opt_prefix(param$long_opt) else ""
    short <- if (!is.null(param$short_opt)) remove_opt_prefix(param$short_opt) else ""
    getopt_param <- list(
      name = name,
      opttype = opttype,
      short = short
    )
  }
  getopt_param
}

#' Convert strings returned by getopt into appropriate R types
#'
#' @param scp Script object
#' @param values List of script values in strings
process_getopt_values <- function(scp, values) {
  opts <- get_options(scp)
  if (length(opts)) {
    for (value_name in names(values)) {
      val <- values[[value_name]]
      opt <- opts[[value_name]]
      values[[value_name]] <- parse_parameter_type(opt, val)
    }
  }
  args <- get_arguments(scp)
  if (length(args)) {
    args_given <- attr(values, "positional")
    args_nargs <- unlist(get_nargs(scp))
    nargs_given <- length(args_given)
    if (Inf %in% args_nargs) {
      stopifnot(sum(args_nargs[args_nargs != Inf]) < nargs_given)
    } else {
      stopifnot(sum(args_nargs) == nargs_given)
    }
    args <- match_positionals_with_arguments(args, nargs_given)
    for (arg_name in names(args)) {
      arg <- args[[arg_name]]
      arg_vals <- args_given[arg$begin:arg$end]
      vals <- NULL
      for (arg_val in arg_vals) {
        vals <- c(vals, parse_parameter_type(arg, arg_val))
      }
      values[[arg_name]] <- vals
    }
  }
  values
}

#' Match the positional args given with those defined in command.
#' At most one argument may allow for a variable number of args (nargs = Inf),
#' so cycle through the args forward (until Inf is encountered), then,
#' if necessary, backward to fully identify the begin/end of the Inf arg.
#' @param args Arguments
#' @param total_nargs Total number of arguments provided by the user
match_positionals_with_arguments <- function(args, total_nargs) {
  arg_begin <- 1
  for (arg_name in names(args)) {  # forward
    args[[arg_name]]$begin <- arg_begin
    if (args[[arg_name]]$nargs == Inf) break
    args[[arg_name]]$end <- arg_begin + args[[arg_name]]$nargs - 1
    arg_begin <- args[[arg_name]]$end + 1
  }
  if (arg_begin <= total_nargs) {
    arg_end <- total_nargs
    for (arg_name in rev(names(args))) {  # backward
      args[[arg_name]]$end <- arg_end
      if (args[[arg_name]]$nargs == Inf) break
      args[[arg_name]]$begin <- arg_end - args[[arg_name]]$nargs + 1
      arg_end <- args[[arg_name]]$begin - 1
    }
  }
  args
}

#' Parse parameter type
#'
#' @param param Script parameter
#' @param value Value of parameter
parse_parameter_type <- function(param, value) {
  if (class(param$type) == "atomic") {
    if ((param$type$class == "logical") && (value == "")) {
      value <- !param$default
    } else {
      value <- as_atomic_type(value, param$type$class)
    }
  } else if (class(param$type) == "interval") {
    stopifnot(is_valid_interval_value(param, value))
    value <- as.numeric(value)
  } else if (class(param$type) == "choice") {
    value <- as_atomic_type(value, typeof(param$type$choices))
    stopifnot(is_valid_choice_value(param, value))
  }
  value
}

#' Check validity of choice value
#'
#' @param param Script parameter
#' @param value Value of parameter
#' @importFrom tools toTitleCase
is_valid_choice_value <- function(param, value) {
  stopifnot(class(param$type) == "choice")
  if (!(value %in% param$type$choices)) {
    comma_delim_choices <- paste(param$type$choices, collapse = ", ")
    stop(paste0(toTitleCase(class(param)), " ", param$name, " does not support value ",
                value, ", choose from ", comma_delim_choices, "."))
  }
  invisible(TRUE)
}


#' Check validity of interval value
#'
#' @param param Script parameter
#' @param value Value of parameter
#' @importFrom tools toTitleCase
is_valid_interval_value <- function(param, value) {
  stopifnot(class(param$type) == "interval")
  int <- param$type
  if ((value < int$lower || int$upper < value) ||
      (int$exclude_lower && value == int$lower) ||
      (int$exclude_upper && value == int$upper)) {
    stop(paste0(toTitleCase(class(param)), " ", param$name, " does not support value ",
                value, ", choose from within ", ifelse(int$exclude_lower, "(", "["),
                int$lower, ", ", int$upper, ifelse(int$exclude_upper, ")", "]"), "."))
  }
  invisible(TRUE)
}

