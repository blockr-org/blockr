new_corrplot_block <- function(data, ...) {
  
  fields <- list(
    method = new_select_field("circle", choices= c("circle", "square", "ellipse", "number", "shade", "color", "pie")),
    type = new_select_field("full", choices= c("full", "lower", "upper")),
    order = new_select_field("original",choices= c("original", "AOE", "FPC", "hclust", "alphabet")),
    title = new_string_field()
  )
  
  new_block(
    fields = fields,
    expr = quote({
      corr_matrix <- stats::cor(select_if(data, is.numeric))
      corrplot::corrplot(corr_matrix, method = .(method), type = .(type),
                         order = .(order), addrect = 2, tl.col = "black", tl.srt = 45,
                         diag = FALSE,
                         title = .(title)
                         
      ) #
    }),
    ...,
    class = c("corrplot_block", "plot_block")
  )
}

corrplot_block <- function(data, ...) {
  initialize_block(new_corrplot_block(data, ...), data)
}

