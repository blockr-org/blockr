#' @param output Shiny output
#' @param result Block result
#' @rdname generate_ui
#' @export
server_output <- function(x, result) {
  UseMethod("server_output", x)
}

#' @rdname generate_ui
#' @export
server_output.block <- function(x, result) {
  DT::renderDT(
    {
      result() |>
        DT::datatable(
          selection = "none",
          options = list(
            pageLength = 5L,
            processing = FALSE
          )
        )
    },
    server = TRUE
  )
}

#' @rdname generate_ui
#' @export
server_output.plot_block <- function(x, result) {
  shiny::renderPlot(result())
}

#' @rdname generate_ui
#' @export
server_output.ggiraph_block <- function(x, result) {
  ggiraph::renderGirafe(result())
}
