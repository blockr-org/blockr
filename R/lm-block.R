#' lm
#'
#' A new transform block.
#'
#' @export
lm_block <- function(data, ...) {
  fields <- list(
    formula =  new_string_field()
  )
  
  blockr::new_block(
    fields = fields,
    expr = quote({
      lm(.(formula), data) |> broom::tidy()
    }),
    ...,
    class = c("lm_block", "transform_block")
  )
}
 