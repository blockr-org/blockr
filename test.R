devtools::load_all()
library(shiny)

stack <- new_stack(
  block_data(),
  block_select()
)

ui <- fluidPage(
  theme = bslib::bs_theme(5L),
  generate_ui(stack)
)

server <- function(input, output) {
  x <- generate_server(stack)
}

shinyApp(ui, server, options = list(port = 3000L))
