#' Parse command-line arguments using standard libc functions
#'
#' @param cmd Command with description and list of params
#' @param args Command line arguments
#' @export
parse_args <- function(cmd, args) {
  getopt_params <- lapply(get_options(cmd), prepare_getopt_param)
  # HACK: libc in getopt assumes first element of args vector is the command
  # call and skips it, so give it empty string to skip
  getopt_args <- c("", args)
  getopt_values <- callgetopt(args = getopt_args, opts = getopt_params)
  if (!is.null(attr(getopt_values, "error"))) {
    stop("Error parsing command arguments")
  }
  values <- process_getopt_values(cmd, getopt_values)
  values
}

#' Translate a single parameter element to the format required for getopt
#'
#' @param param List containing parameter details
#' @export
prepare_getopt_param <- function(param) {
  if (class(param) == "option") {
    if (param$type == "logical") {
      opttype <- "flag"
    } else {
      opttype <- "required"
    }
    name <- ifelse(!is.null(param$long_opt), remove_prefix(param$long_opt), "")
    short <- ifelse(!is.null(param$short_opt), remove_prefix(param$short_opt), "")
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
#' @param cmd Command list
#' @param values List of script values in strings
process_getopt_values <- function(cmd, values) {
  opts <- get_options(cmd)
  if (length(opts)) {
    for (value_name in names(values)) {
      val <- values[[value_name]]
      opt <- opts[[value_name]]
      if (val == "" && opt$type == "logical") {
        values[[value_name]] <- !opt$default
      } else {
        values[[value_name]] <- as.type(val, opt$type)
      }
    }
  }

  # Match the positional args given with those defined in command.
  # At most one argument may allow for a variable number of args (nargs = Inf),
  # so cycle through the args forward (until Inf is encountered), then,
  # if necessary, backward to fully identify the begin/end of the Inf arg.
  args <- get_arguments(cmd)
  if (length(args)) {
    args_given <- attr(values, "positional")
    args_nargs <- unlist(get_nargs(cmd))
    nargs_given <- length(args_given)
    if (Inf %in% args_nargs) {
      stopifnot(sum(args_nargs[args_nargs != Inf]) < nargs_given)
    } else {
      stopifnot(sum(args_nargs) == nargs_given)
    }
    arg_begin <- 1
    for (arg_name in names(args)) {  # forward
      args[[arg_name]]$begin <- arg_begin
      if (args[[arg_name]]$nargs == Inf) break
      args[[arg_name]]$end <- arg_begin + args[[arg_name]]$nargs - 1
      arg_begin <- args[[arg_name]]$end + 1
    }
    if (arg_begin <= nargs_given) {
      arg_end <- nargs_given
      for (arg_name in rev(names(args))) {  # backward
        args[[arg_name]]$end <- arg_end
        if (args[[arg_name]]$nargs == Inf) break
        args[[arg_name]]$begin <- arg_end - args[[arg_name]]$nargs + 1
        arg_end <- args[[arg_name]]$begin - 1
      }
    }
    for (arg_name in names(args)) {
      arg <- args[[arg_name]]
      values[[arg_name]] <- args_given[arg$begin:arg$end]
    }
  }

  values
}


