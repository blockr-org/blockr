#' UI
#'
#' Generic for UI generation
#'
#' @param x Object for which to generate UI components
#' @param ... Generic consistency
#' @param .hidden Whether to initialise the block hidden.
#'
#' @export
generate_ui <- function(x, ...) {
  UseMethod("generate_ui")
}

#' @rdname generate_ui
#' @export
ui_fields <- function(x, ...) {
  UseMethod("ui_fields", x)
}

#' @rdname generate_ui
#' @param inputs_hidden For styling purposes: CSS class to apply
#' when the block is collapsed.
#' @export
ui_fields.block <- function(x, ns, inputs_hidden, ...) {
  fields <- Map(
    ui_input,
    x,
    id = chr_ply(names(x), ns),
    name = names(x)
  )

  layout <- attr(x, "layout")

  div(
    class = sprintf("block-inputs %s", inputs_hidden),
    layout(fields)
  )
}

#' @rdname generate_ui
#' @export
block_body <- function(x, ...) {
  UseMethod("block_body", x)
}

#' @rdname generate_ui
#' @export
block_body.block <- function(x, ns, inputs_hidden, ...) {

  result_id <- ns("outputCollapse")

  loading_class <- "d-none"
  if (inputs_hidden != "") {
    loading_class <- ""
  }

  tagList(
    tags$a(
      class = "text-decoration-none block-output-toggle",
      href = sprintf("#%s", result_id),
      iconOutput()
    ),
    ui_fields(x, ns, inputs_hidden),
    div(
      class = sprintf("%s block-output", inputs_hidden),
      id = result_id,
      uiOutputBlock(x, ns),
      div(
        class = sprintf(
          "block-loading d-flex justify-content-center %s",
          loading_class
        ),
        div(
          class = "spinner-border text-primary",
          role = "status",
          span(
            class = "visually-hidden",
            "Loading..."
          )
        )
      )
    )
  )
}

#' @rdname generate_ui
#' @export
block_code <- function(x, ...) {
  UseMethod("block_code", x)
}

#' @rdname generate_ui
#' @export
block_code.block <- function(x, ns, inputs_hidden, ...) {

  code_id <- ns("codeCollapse")

  div(
    class = sprintf("%s block-output", inputs_hidden),
    tags$a(
      class = "text-decoration-none block-code-toggle",
      `data-bs-toggle` = "collapse",
      href = sprintf("#%s", code_id),
      `aria-expanded` = "false",
      `aria-controls` = code_id,
      iconCode()
    ),
    div(
      class = "collapse block-code",
      id = code_id,
      uiCode(x, ns)
    )
  )
}

#' @rdname generate_ui
#' @export
block_header <- function(x, ...) {
  UseMethod("block_header", x)
}

#' @importFrom shiny tags div p
#' @export
block_header.block <- function(x, ns, hidden_class, ...) {
  title <- class(x)[1] |>
    (\(.) gsub("_.*$", "", .))() |>
    tools::toTitleCase()

  div(
    class = sprintf("m-0 card-title block-title %s", hidden_class),
    div(
      class = "d-flex",
      div(
        class = "flex-grow-1",
        p(
          span(icon("cube"), class = "text-muted"),
          title,
          class = "fw-bold"
        )
      ),
      div(
        class = "flex-grow-1",
        span(
          class = "block-feedback text-muted",
          span(textOutput(ns("nrow"), inline = TRUE), class = "fw-bold"),
          "rows |",
          class = "block-feedback text-muted",
          span(textOutput(ns("ncol"), inline = TRUE), class = "fw-bold"),
          "cols"
        )
      ),
      div(
        class = "block-tools flex-shrink-1"
      )
    )
  )
}

#' @rdname generate_ui
#' @export
block_remove <- function(x, ...) {
  UseMethod("block_remove", x)
}

#' @rdname generate_ui
#' @export
block_remove.block <- function(x, id, ...) {
  actionLink(
    id,
    icon("trash"),
    class = "text-decoration-none block-remove",
  )
}

#' @param id UI IDs
#' @rdname generate_ui
#' @export
generate_ui.block <- function(x, id, ..., .hidden = !getOption("BLOCKR_DEV", FALSE)) {
  stopifnot(...length() == 0L)

  ns <- NS(id)

  block_class <- "block"
  inputs_hidden <- ""
  hidden_class <- ""
  if (.hidden) {
    hidden_class <- "d-none"
    inputs_hidden <- hidden_class
    block_class <- sprintf("%s d-none", block_class)
  }

  div(
    class = block_class,
    `data-block-type` = paste0(class(x), collapse = ","),
    `data-value` = ns("block"),
    div(
      class = "card shadow-sm p-2 mb-2 border",
      div(
        class = "card-body p-1",
        block_header(x, ns, hidden_class),
        div(class = "block-validation"),
        block_body(x, ns, inputs_hidden),
        block_code(x, ns, inputs_hidden)
      )
    )
  )
}

#' @rdname generate_ui
#' @export
generate_ui.stack <- function(
  x,
  id = NULL,
  ...
) {
  stopifnot(...length() == 0L)

  id <- if (is.null(id)) attr(x, "name") else id
  ns <- NS(id)

  tagList(
    div(
      class = "card stack border",
      id = id,
      stack_header(x, id, ns),
      div(
        class = "card-body p-1",
        id = sprintf("%s-body", id),
        lapply(x, \(b) {
          inject_remove_button(ns, b)
        })
      )
    ),
    blockrDeps()
  )
}

#' Inject remove button into block header
#'
#' This has to be called from the stack parent
#' namespace. This can also be called dynamically when
#' inserting a new block within a stack.
#'
#' @param ns Stack namespace.
#' @param b Current block.
#' @param .hidden Internal parameter. Default to FALSE
#'
#' @keywords internal
inject_remove_button <- function(ns, b, .hidden = !getOption("BLOCKR_DEV", FALSE)) {
  block_id <- attr(b, "name")
  tmp <- generate_ui(b, id = ns(block_id), .hidden = FALSE)
  # Remove button now belongs to the stack namespace!
  htmltools::tagQuery(tmp)$
    find(".block-tools")$
    prepend(block_remove(b, ns(sprintf("remove-block-%s", block_id))))$
  allTags()
}

#' @rdname generate_ui
#' @export
stack_header <- function(x, ...) {
  UseMethod("stack_header", x)
}

#' @importFrom shiny icon tags div
stack_header.stack <- function(x, title, ns, ...) {
  div(
    class = "card-header",
    div(
      class = "d-flex",
      div(
        class = "flex-grow-1 d-inline-flex",
        span(icon("cubes"), class = "text-muted"),
        span(get_title(x), class = "stack-title cursor-pointer")
      ),
      div(
        class = "flex-shrink-1",
        div(
          class = "ps-1 py-2",
          # TO DO: move it to workspace (needs other PR).
          #actionLink(
          #  ns("remove"),
          #  "",
          #  class = "text-decoration-none stack-remove",
          #  iconTrash()
          #),
          actionLink(
            ns("copy"),
            class = "text-decoration-none stack-copy-code",
            `data-bs-toggle` = "tooltip",
            `data-bs-title` = "Copy code",
            iconCode()
          ),
          tags$a(
            class = "text-decoration-none stack-edit-toggle",
            iconEdit()
          )
        )
      )
    )
  )
}

#' @param name Field name
#' @rdname generate_ui
#' @export
ui_input <- function(x, id, name) {
  UseMethod("ui_input", x)
}

#' @rdname generate_ui
#' @export
ui_input.string_field <- function(x, id, name) {
  textInput(input_ids(x, id), name, value(x))
}

#' @rdname generate_ui
#' @export
ui_input.select_field <- function(x, id, name) {
  selectizeInput(
    input_ids(x, id), name, value(x, "choices"), value(x), value(x, "multiple"),
    options = list(
      dropdownParent = "body",
      placeholder = "Please select an option below"
    )
  )
}

#' @rdname generate_ui
#' @export
ui_input.switch_field <- function(x, id, name) {
  bslib::input_switch(input_ids(x, id), name, value(x))
}

#' @rdname generate_ui
#' @export
ui_input.numeric_field <- function(x, id, name) {
  numericInput(
    input_ids(x, id), name, value(x), value(x, "min"), value(x, "max")
  )
}

#' @rdname generate_ui
#' @export
ui_input.submit_field <- function(x, id, name) {
  actionButton(
    input_ids(x, id),
    name,
    icon = icon("play"),
    class = "btn btn-success mt-4"
  )
}

#' @rdname generate_ui
#' @export
input_ids <- function(x, ...) {
  UseMethod("input_ids", x)
}

#' @rdname generate_ui
#' @export
input_ids.block <- function(x, ...) {
  Map(input_ids, x, names(x))
}

#' @rdname generate_ui
#' @export
input_ids.field <- function(x, name, ...) {
  name
}

#' @rdname generate_ui
#' @export
input_ids.list_field <- function(x, name, ...) {
  sub_names <- names(value(x, "sub_fields"))
  set_names(paste0(name, "_", sub_names), sub_names)
}

#' @rdname generate_ui
#' @export
ui_input.variable_field <- function(x, id, name) {
  field <- validate_field(
    materialize_variable_field(x)
  )

  div(
    id = paste0(id, "_cont"),
    ui_input(field, id, name)
  )
}

#' @rdname generate_ui
#' @export
ui_input.range_field <- function(x, id, name) {
  sliderInput(
    input_ids(x, id), name, value(x, "min"), value(x, "max"), value(x)
  )
}

#' @rdname generate_ui
#' @export
ui_input.hidden_field <- function(x, id, name) {
  NULL
}

#' @rdname generate_ui
#' @export
ui_input.list_field <- function(x, id, name) {
  fields <- lapply(
    update_sub_fields(value(x, "sub_fields"), value(x)),
    validate_field
  )

  # TODO: indicate nesting of fields, nice version of
  # `paste0(name, "_", names(fields))` instead of just `names(fields)`

  args <- c(
    list(id = paste0(id, "_cont")),
    map(ui_input, fields, input_ids(x, id), names(fields))
  )

  do.call(div, args)
}

#' @param session Shiny session
#' @rdname generate_ui
#' @export
ui_update <- function(x, session, id, name) {
  UseMethod("ui_update", x)
}

#' @rdname generate_ui
#' @export
ui_update.string_field <- function(x, session, id, name) {
  updateTextInput(session, input_ids(x, id), name, value(x))
}

#' @rdname generate_ui
#' @export
ui_update.select_field <- function(x, session, id, name) {
  updateSelectInput(
    session, input_ids(x, id), name, value(x, "choices"), value(x)
  )
}

#' @rdname generate_ui
#' @export
ui_update.switch_field <- function(x, session, id, name) {
  bslib::update_switch(input_ids(x, id), name, value(x), session)
}

#' @rdname generate_ui
#' @export
ui_update.variable_field <- function(x, session, id, name) {
  ns <- session$ns
  ns_id <- ns(id)

  removeUI(
    selector = paste0("#", ns_id, "_cont", " > div"),
    session = session
  )

  field <- validate_field(
    materialize_variable_field(x)
  )

  insertUI(
    selector = paste0("#", ns_id, "_cont"),
    ui = ui_input(field, ns_id, name),
    session = session
  )
}

#' @rdname generate_ui
#' @export
ui_update.range_field <- function(x, session, id, name) {
  updateSliderInput(
    session, input_ids(x, id), name, value(x), value(x, "min"), value(x, "max")
  )
}

#' @rdname generate_ui
#' @export
ui_update.numeric_field <- function(x, session, id, name) {
  updateNumericInput(
    session, input_ids(x, id), name, value(x), value(x, "min"), value(x, "max")
  )
}

#' @rdname generate_ui
#' @export
ui_update.submit_field <- function(x, session, id, name) {
  updateActionButton(
    session,
    input_ids(x, id),
    name
  )
}

#' @rdname generate_ui
#' @export
ui_update.hidden_field <- function(x, session, id, name) {
  NULL
}

#' @rdname generate_ui
#' @export
ui_update.list_field <- function(x, session, id, name) {
  ns <- session$ns
  ns_id <- ns(id)

  removeUI(
    selector = paste0("#", ns_id, "_cont", " > div"),
    multiple = TRUE,
    session = session
  )

  fields <- lapply(
    update_sub_fields(value(x, "sub_fields"), value(x)),
    validate_field
  )

  insertUI(
    selector = paste0("#", ns_id, "_cont"),
    ui = do.call(
      tagList,
      map(
        ui_input, fields, input_ids(x, ns_id), paste0(name, "_", names(fields))
      )
    ),
    session = session
  )
}

#' Custom card container
#' @keywords internal
div_card <- function(..., title = NULL, value) {
  panel_tag <- bslib::accordion_panel(
    title = if (not_null(title)) title,
    value = value,
    ...
  )
  tagAppendAttributes(panel_tag, class = "block")
}

#' Custom code container
#' @keywords internal
custom_verbatim_output <- function(id) {
  tmp <- shiny::verbatimTextOutput(id)
  tmp$attribs$style <- "max-height: 400px; overflow-y: scroll"
  tmp
}
#' @param ns Output namespace
#' @rdname generate_ui
#' @export
uiOutputBlock <- function(x, ns) {
  UseMethod("uiOutputBlock", x)
}

#' @rdname generate_ui
#' @export
uiOutputBlock.block <- function(x, ns) {
  DT::dataTableOutput(ns("res"))
}

#' @rdname generate_ui
#' @export
uiOutputBlock.plot_block <- function(x, ns) {
  shiny::plotOutput(ns("plot"))
}

#' @rdname generate_ui
#' @export
uiOutputBlock.ggiraph_block <- function(x, ns) {
  ggiraph::girafeOutput(ns("plot"))
}

#' @rdname generate_ui
#' @export
uiCode <- function(x, ns) {
  UseMethod("uiCode", x)
}

#' @rdname generate_ui
#' @export
uiCode.block <- function(x, ns) {
  div(
    class = "position-relative",
    actionButton(
      ns("copy"),
      class = "btn-sm btn-info position-absolute top-0 end-0 block-copy-code",
      "",
      icon = icon("copy")
    ),
    shiny::verbatimTextOutput(ns("code"))
  )
}

#' @importFrom shiny icon
iconCode <- function() {
  icon("code")
}

#' @importFrom shiny icon
iconEdit <- function() {
  icon("chevron-up")
}

#' @importFrom shiny icon
iconOutput <- function() {
  icon("chevron-up")
}

#' @importFrom shiny icon
iconTrash <- function() {
  icon("trash")
}
