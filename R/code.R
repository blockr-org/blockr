#' @param state Block state
#' @rdname generate_ui
#' @export
server_code <- function(x, env) {
  UseMethod("server_code", x)
}

#' @rdname generate_ui
#' @export
server_code.block <- function(x, env) {
  shiny::renderPrint({
    generate_code(x, env)
  })
}

#' @rdname new_block
#' @export
generate_code <- function(x, env) {
  UseMethod("generate_code")
}

#' @rdname new_block
#' @export
generate_code.block <- function(x, env) {
  do.call("substitute", list(attr(x, "expr"), env))
}
