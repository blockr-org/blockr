#' @rdname new_field
#' @export
new_namedchar_field <- function(value = character(), ...) {
  new_field(value, ..., class = c("namedchar_field"))
}

#' @rdname generate_ui
#' @export
ui_input.namedchar_field <- function(x, id, name) {
  uiOutput(input_ids(x, id))
}

validate_field.namedchar_field <- function(x) {
  val <- value(x)
  x
}




# DISCUSS Module based fields need a new method to draw their ui,
# because ui_input contains `uiOutput(input_ids(x, id))` only.

#' @export
module_field_ui <- function(x, id, ...) {
  UseMethod("module_field_ui", x)
}

module_field_ui.namedchar_field <- function(x, id, ...) {
  ace_module_ui(id, exprs_init = value(x, "value"))
}



# 'x' for dispatch only?
#' @export
module_field_server <- function(x, id, ...) {
  UseMethod("module_field_server", x)
}

module_field_server.namedchar_field <- function(x, id, ...) {
  ace_module_server(id)
}



# perhaps a secondary `module_field` class?

# #' @rdname generate_ui
# #' @export
# ui_input.module_field <- function(x, id, name) {
#   uiOutput(input_ids(x, id))
# }
