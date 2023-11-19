#' Server
#'
#' Generic for server generation
#'
#' @param x Object for which to generate a [moduleServer()]
#' @param ... Generic consistency
#'
#' @export
#' @import shiny
generate_server <- function(x, ...) {
  UseMethod("generate_server")
}

#' @rdname generate_server
#' @export
generate_server.block <- function(x, ...) {
  id <- attr(x, "name")

  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      block_reactive <- reactiveVal(x)

      # we compute named list of inputs for this block
      # this is to be used as environment for the
      # block code evaluation
      env <- reactive({
        values <- x |>
          names() |>
          lapply(\(x) input[[x]])

        names(values) <- names(x)
        return(values)
      })

      out_dataset <- reactive({
        evaluate_block(x, env())
      })

      output$res <- server_output(x, out_dataset)
      output$code <- server_code(x, env)

      output$nrow <- renderText({
        prettyNum(nrow(out_dataset()), big.mark = ",")
      })

      output$ncol <- renderText({
        prettyNum(ncol(out_dataset()), big.mark = ",")
      })

      out_dataset
    }
  )
}

#' @param in_dat Reactive input data
#' @rdname generate_server
#' @export
generate_server.transform_block <- function(x, in_dat, id, ...) {
  obs_expr <- function(x) {
    splice_args(
      list(in_dat(), ..(args)),
      args = lapply(unlst(input_ids(x)), quoted_input_entry)
    )
  }

  set_expr <- function(x) {
    splice_args(
      blk(update_fields(blk(), session, in_dat(), ..(args))),
      args = rapply(input_ids(x), quoted_input_entries, how = "replace")
    )
  }

  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      blk <- reactiveVal(x)

      o <- observeEvent(
        eval(obs_expr(blk())),
        secure(eval(set_expr(blk()))),
        ignoreInit = TRUE
      )

      # For submit blocks like filter, summarise,
      # join that can have computationally intense tasks
      # and have nested fields, we require to click on
      # the action button before doing anything.
      out_dat <- if ("submit_block" %in% class(x)) {
        eventReactive(input$submit, {
          evaluate_block(blk(), data = in_dat())
        })
      } else {
        reactive(
          evaluate_block(blk(), data = in_dat())
        )
      }

      output$res <- server_output(x, out_dat, output)
      output$code <- server_code(x, blk, output)

      output$nrow <- renderText({
        prettyNum(nrow(out_dat()), big.mark = ",")
      })

      output$ncol <- renderText({
        prettyNum(ncol(out_dat()), big.mark = ",")
      })

      # Cleanup module inputs (UI and server side)
      # and observer
      observeEvent(input$remove, {
        message(sprintf("CLEANING UP BLOCK %s", id))
        remove_shiny_inputs(id = id, input)
        o$destroy()
        session$userData$is_cleaned(TRUE)
      })

      out_dat
    }
  )
}

#' @param in_dat Reactive input data
#' @rdname generate_server
#' @export
generate_server.plot_block <- function(x, in_dat, id, ...) {
  obs_expr <- function(x) {
    splice_args(
      list(in_dat(), ..(args)),
      args = lapply(unlst(input_ids(x)), quoted_input_entry)
    )
  }

  set_expr <- function(x) {
    splice_args(
      blk(update_fields(blk(), session, in_dat(), ..(args))),
      args = rapply(input_ids(x), quoted_input_entries, how = "replace")
    )
  }

  shiny::moduleServer(
    id,
    function(input, output, session) {
      blk <- shiny::reactiveVal(x)

      o <- shiny::observeEvent(
        eval(obs_expr(blk())),
        eval(set_expr(blk())),
        ignoreInit = TRUE
      )

      out_dat <- shiny::reactive({
        evaluate_block(blk(), data = in_dat())
      })

      output$plot <- server_output(x, out_dat, output)
      output$code <- server_code(x, blk, output)

      output$nrow <- renderText({
        prettyNum(nrow(out_dat()), big.mark = ",")
      })

      output$ncol <- renderText({
        prettyNum(ncol(out_dat()), big.mark = ",")
      })

      # Cleanup module inputs (UI and server side)
      # and observer
      observeEvent(input$remove, {
        message(sprintf("CLEANING UP BLOCK %s", id))
        remove_shiny_inputs(id = id, input)
        o$destroy()
        session$userData$is_cleaned(TRUE)
      })
    }
  )
}

#' @param in_dat Reactive input data
#' @rdname generate_server
#' @export
generate_server.ggiraph_block <- generate_server.plot_block

#' @rdname generate_server
#' @param id Unique module id. Useful when the stack is called as a module.
#' @param new_blocks For dynamically inserted blocks.
#' @export
generate_server.stack <- function(x, id = NULL, new_blocks = NULL, ...) {
  stopifnot(...length() == 0L)

  if (is.null(id)) {
    id <- x$name
  }

  moduleServer(
    id = id,
    function(input, output, session) {
      blocks <- lapply(x$blocks, generate_server)

      observe({
        session$sendCustomMessage(
          "blockr-bind-stack",
          list(stack = session$ns(NULL))
        )
      })

      blocks
    }
  )
}

#' Init blocks server
#' @keywords internal
init_blocks <- function(x, vals, session) {
  observeEvent(TRUE, {
    session$userData$stack <- vals$stack
    for (i in seq_along(x)) {
      vals$blocks[[i]] <- init_block(i, vals)
    }
  })
}

#' Init a single block
#'
#' Useful for \link{init_blocks} but also
#' to be called after \link{add_block}.
#'
#' @param i Block position
#' @param vals Reactive values containing the stack.
#'
#' @keywords internal
init_block <- function(i, vals) {
  generate_server(
    vals$stack[[i]],
    in_dat = if (i == 1) {
      # No data for first block
      NULL
    } else {
      # Data from previous block
      vals$blocks[[i - 1]]
    },
    id = attr(vals$stack[[i]], "name")
  )
}

#' Cleanup module inputs
#' @keywords internal
remove_shiny_inputs <- function(id, .input) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}
