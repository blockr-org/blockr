#' Evaluate block
#' @export
evaluate_block <- function(x, evn, ...) {
  UseMethod("evaluate_block")
}

#' @export
evaluate_block.data_block <- function(x, env, ...) {
  stopifnot(...length() == 0L)
  eval(attr(x, "expr"), env)
}
