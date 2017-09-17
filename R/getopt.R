#' Parse command-line arguments using standard libc functions
#'
#' @param cmd command definition, of the form list(a='long-a',b='long-b')
#' @param argv arguments to parse (excluding command name)
#' @export
parse_args <- function(cmd, argv = commandArgs(TRUE)) {
  stopifnot(class(cmd) == 'command')

  opt_params = lapply(X = cmd$params, FUN = one_getopt_param)
  # HACK HACK HACK - append empty string to start of args so that libc doesn't skip first element
  opts <- callgetopt(args = c('', argv), opts = opt_params)
  if (!is.null(attr(opts, 'error'))) {
    stop('Error parsing command arguments')
  }
  opts
}

#' Translate a single parameter element to the format required for parse_args
#'
#' @param param a list containing parameter details
#' @export
one_getopt_param <- function(param) {
  lopt <- sopt <- ''
  if (!is.null(param$long_opt)) {
    lopt <- remove_prefix(param$long_opt)
  }
  if (!is.null(param$short_opt)) {
    sopt <- remove_prefix(param$short_opt)
  }
  opttype <- 'required' # 'optional' #
  if (param$type == 'logical') {
    opttype <- 'flag'
  }
  list(name=lopt, opttype=opttype, short=sopt)
}

