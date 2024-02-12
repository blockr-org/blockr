
#' ggscatterstats
#'
#' A new transform block.
#'
#' @export
ggscatterstats_block <- function(data, ...) {
  types <- c(
    "parametric",
    "nonparametric",
    "robust",
    "bayes"
  )
  num_cols <- colnames(dplyr::select_if(data, is.numeric))
  blockr::new_block(
    expr = quote({
      ggstatsplot::ggscatterstats(data = data,
                     x = .(x),
                     y = .(y),
                     type = .(type),
                   #  conf.level = .(conf.level),
                     xlab = .(xlab),
                     ylab = .(ylab),
                     title = .(title))
    }),
    fields = list(
      x = blockr::new_select_field(num_cols[1], num_cols),
      y = blockr::new_select_field(num_cols[1], num_cols),
      type = blockr::new_select_field(types[1], types),
    #  conf.level = blockr::new_numeric_field(0.95),
      xlab = new_string_field(""),
      ylab = new_string_field(""),
      title = new_string_field("")
    ),
    ...,
    class = c("ggscatterstats_block", "plot_block")
  )
}

 