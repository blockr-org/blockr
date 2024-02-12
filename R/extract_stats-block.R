#' extract_stats
#'
#' A new transform block.
#'
#' @export
extract_stats_block <- function(data, ...) {
  fields <- list(
    n_rows = new_numeric_field(10, 10, nrow(data))
  )

  new_block(
    fields = fields,
    expr = quote({
      head(data, .(n_rows))
    }),
    ...,
    class = c("extract_stats_block", "transform_block")
  )
}
