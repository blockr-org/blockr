library(shiny)
library(blockr.data)
library(blockr)

register_block(
  constructor = new_upload_block,
  name = "upload block",
  description = "upload a single file",
  classes = c("upload_block", "data_block"),
  input = NA_character_,
  output = "data.frame"
)
register_block(
  constructor = lm_block,
  name = "linear model block",
  description = "Enter a formula",
  classes = c("lm_block", "transform_block"),
  input = "data.frame",
  output = "data.frame"
)


register_block(
  constructor = mutate_block,
  name = "mutate block",
  description = "Creates a linear transformation",
  classes = c("mutate_block", "transform_block"),
  input = "data.frame",
  output = "data.frame"
)


## TODO: new join block not working 
# register_block(
#   constructor = new_join_block,
#   name = "join block",
#   description = "upload a single file to join",
#   classes = c("join_block", "transform_block"),
#   input = "character",
#   output = "data.frame"
# )




stack <- new_stack()
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
