library(shiny)
devtools::load_all()
library(blockr.ggplot2)

stack <- new_stack(data_block)

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5),
  actionButton("btn", "Serialise"),
  uiOutput("stacks")
)

server <- function(input, output, session) {
  output$stacks <- renderUI({
    generate_ui(stack)
  })

  generate_server(stack)

  observeEvent(input$btn, {
    print("---------------------------------")
    print(to_json())
  })
}

shinyApp(ui, server)
