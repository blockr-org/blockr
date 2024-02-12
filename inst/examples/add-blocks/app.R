library(shiny)
library(blockr.data)
library(blockr)

stack <- new_stack()

demo_data_block <- function(...) {
  initialize_block(
    new_data_block(

      ...,
      selected = "iris"
    )
  )
}
blockr::register_block(
  constructor = demo_data_block,
  name = "demo data block",
  description = "data block",
  classes = c("demo_data_block", "data_block"),
  input = "data.frame",
  output = "data.frame"
)

register_block(
  constructor = new_upload_block,
  name = "upload block",
  description = "upload a single file",
  classes = c("upload_block", "data_block"),
  input = NA_character_,
  output = "data.frame"
)
num_cols <- function(data) {
  colnames(dplyr::select_if(data, is.numeric))
}
char_cols <- function(data) {
  colnames(dplyr::select_if(data, \(x) is.character(x) | is.factor(x)))
}

#' ggscatterstats
#'
#' A new transform block.
#'
#' @export
new_ggscatterstats_block <- function(data, ...) {
  types <- c(
    "parametric",
    "nonparametric",
    "robust",
    "bayes"
  )

  new_block(
    expr = quote(
      ggscatterstats(
        x = .(x),
        y = .(y),
        type = .(type),
        conf.level = .(conf.level)
      )
    ),
    fields = list(
      x = new_select_field(num_cols(data)[1], num_cols(data)),
      y = new_select_field(num_cols(data)[2], num_cols(data)),
      type = new_select_field(types[1], types),
      conf.level = new_numeric_field(0.95, min = 0, max = 1)
    ),
    class = c("ggscatterstats_block", "plot_block")
  )
}

new_label_block <- function(data, ...) {
  new_block(
    fields = list(
      xlab = new_string_field(num_cols(data$data)[1]),
      ylab = new_string_field(num_cols(data$data)[2]),
      title = new_string_field("")
    ),
    expr = quote(
      ggplot2::labs(x = .(xlab), y = .(ylab), title = .(title))
    ),
    class = c("plot_layer_block", "plot_block")
  )
}

label_block <- function(data, ...) {
  initialize_block(new_label_block(data, ...), data)
}

blockr::register_block(
  constructor = label_block,
  name = "label plot",
  description = "label plots",
  classes = c("label_block", "plot_block", "plot_layer_block"),
  input = "data.plot",
  output = "data.plot"
)

# blockr::register_block(
#   constructor = ggscatterstats_block,
#   name = "scatter plot",
#   description = "scatter with stats",
#   classes = c("ggscatterstats_block", "plot_block"),
#   input = "data.frame",
#   output = "data.plot"
# )

register_block(
  constructor = mutate_block,
  name = "mutate block",
  description = "Creates a linear transformation",
  classes = c("mutate_block", "transform_block"),
  input = "data.frame",
  output = "data.frame"
)

extract_stats_block <- function(data, ...) {
  fields <- list(
    attribute = new_select_field("subtitle_data", choices = c(
      "subtitle_data", "caption_data", "pairwise_comparisons_data", "descriptive_data", "one_sample_data",
      "tidy_data", "glance_data"
    ))
  )

  new_block(
    fields = fields,
    expr = quote({
      stats <- ggstatsplot::extract_stats(data)
      as.data.frame(stats[.(attribute)])
    }),
    ...,
    class = c("extract_stats_block", "transform_block")
  )
}
# blockr::register_block(
#   constructor = extract_stats_block,
#   name = "extract stats",
#   description = "extract statistics from plot",
#   classes = c("extract_stats_block", "transform_block"),
#   input = "plot",
#   output = "data.frame"
# )


ggbetweenstats_block <- function(data, ...) {
  types <- c(
    "parametric",
    "nonparametric",
    "robust",
    "bayes"
  )


  blockr::new_block(
    expr = quote({
      ggstatsplot::ggbetweenstats(
        data = data,
        x = .(x),
        y = .(y),
        type = .(type),
        conf.level = .(conf.level)
      )
    }),
    fields = list(
      x = blockr::new_select_field(char_cols(data)[1], char_cols(data)),
      y = blockr::new_select_field(num_cols(data)[1], num_cols(data)),
      type = blockr::new_select_field(types[1], types),
      conf.level = blockr::new_numeric_field(0.95, min = 0, max = 1)
    ),
    class = c("ggbetweenstats_block", "plot_block")
  )
}

# 
# blockr::register_block(
#   constructor = ggbetweenstats_block,
#   name = "between stats plot",
#   description = "scatter with stats",
#   classes = c("ggbetweenstats_block", "plot_block"),
#   input = "data.frame",
#   output = "data.plot"
# )
# 








new_ggplot_block <- function(data, ...) {
  data_cols <- function(data) colnames(data)
  new_block(
    fields = list(
      x = new_select_field(colnames(data)[1], data_cols, type = "name"),
      y = new_select_field(colnames(data)[2], data_cols, type = "name")
    ),
    expr = quote(
      ggplot(mapping = aes(x = .(x), y = .(y)))
    ),
    class = c("ggplot_block", "plot_block"),
    ...
  )
}

ggplot_block <- function(data, ...) {
  initialize_block(new_ggplot_block(data, ...), data)
}

new_geompoint_block <- function(data, ...) {

  new_block(
    fields = list(),
    expr = quote(geom_point()),
    class = c("plot_layer_block", "plot_block"),
    ...
  )
}

geompoint_block <- function(data, ...) {
  initialize_block(new_geompoint_block(data, ...), data)
}


new_geomsmooth_block <- function(data, ...) {
  new_block(
    fields = list(
      color = new_select_field("blue", c("blue", "green", "red"))
    ),
    expr = quote(
      geom_smooth(color = .(color))
    ),
    class = c("plot_layer_block", "plot_block"),
    ...
  )
}

geomsmooth_block <- function(data, ...) {
  initialize_block(new_geomsmooth_block(data, ...), data)
}

register_block(
  constructor = geomsmooth_block,
  name = "geom smooth block",
  description = "Create the ggplot smooth block",
  classes = c("plot_layer_block", "plot_block"),
  input = "data.plot",
  output = "data.plot"
)

register_block(
  constructor = ggplot_block,
  name = "ggplot block",
  description = "Create the ggplot",
  classes = c("ggplot_block", "plot_block"),
  input = "data.frame",
  output = "data.plot"
)

register_block(
  constructor = geompoint_block,
  name = "geom point block",
  description = "Create the geom point",
  classes = c("plot_layer_block", "plot_block"),
  input = "data.plot",
  output = "data.plot"
)

 


shinyApp(
  ui = bslib::page_fluid(
    add_block_ui(),
    generate_ui(stack, id = "mystack")
  ),
  server = function(input, output, session) {
    vals <- reactiveValues(new_block = NULL)
    stack <- generate_server(
      stack,
      id = "mystack",
      new_block = reactive(vals$new_block)
    )

    observeEvent(input$add, {
      vals$new_block <- NULL
      # Always append to stack
      loc <- length(stack$blocks)
      block <- available_blocks()[[input$selected_block]]
      # add_block expect the current stack, the block to add and its position
      # (NULL is fine for the position, in that case the block will
      # go at the end)
      vals$new_block <- list(
        block = block,
        position = loc
      )
    })
  }
)
