test_that("workspace", {
  withr::local_options(BLOCKR_LOG_LEVEL = "error")

  withr::defer(clear_workspace_stacks())

  expect_s3_class(get_workspace(), "workspace")

  stack1 <- new_stack(
    new_dataset_block,
    new_filter_block
  )

  set_workspace(stack1 = stack1)

  expect_length(get_workspace_stacks(), 1L)
  expect_identical(get_workspace_title(), "")
  expect_identical(get_workspace_settings(), list())
  expect_identical(list_workspace_stacks(), "stack1")
  expect_s3_class(get_workspace_stack("stack1"), "stack")

  stack2 <- new_stack(
    new_dataset_block,
    new_select_block
  )

  add_workspace_stack("stack2", stack2)

  expect_length(get_workspace_stacks(), 2L)
  expect_identical(get_workspace_title(), "")
  expect_identical(get_workspace_settings(), list())
  expect_setequal(list_workspace_stacks(), c("stack1", "stack2"))
  expect_s3_class(get_workspace_stack("stack1"), "stack")
  expect_s3_class(get_workspace_stack("stack2"), "stack")

  expect_warning(
    add_workspace_stack("stack2", stack2)
  )

  expect_length(get_workspace_stacks(), 2L)
  expect_setequal(list_workspace_stacks(), c("stack1", "stack2"))

  add_workspace_stack("stack2", stack2, force = TRUE)

  expect_length(get_workspace_stacks(), 2L)
  expect_setequal(list_workspace_stacks(), c("stack1", "stack2"))

  set_workspace_stack("stack2", stack2)

  expect_length(get_workspace_stacks(), 2L)
  expect_setequal(list_workspace_stacks(), c("stack1", "stack2"))

  expect_warning(
    set_workspace_stack("stack3", new_stack())
  )

  expect_length(get_workspace_stacks(), 3L)
  expect_setequal(list_workspace_stacks(), c("stack1", "stack2", "stack3"))

  rm_workspace_stacks(c("stack2", "stack3"))

  expect_warning(
    rm_workspace_stack("stack2")
  )

  expect_length(get_workspace_stacks(), 1L)
  expect_identical(get_workspace_title(), "")
  expect_identical(get_workspace_settings(), list())
  expect_identical(list_workspace_stacks(), "stack1")
  expect_s3_class(get_workspace_stack("stack1"), "stack")

  clear_workspace_stacks(TRUE)

  set_workspace(stack1 = stack1, stack2 = stack2)

  expect_length(get_workspace_stacks(), 2L)
  expect_identical(get_workspace_title(), "")
  expect_identical(get_workspace_settings(), list())
  expect_setequal(list_workspace_stacks(), c("stack1", "stack2"))
  expect_s3_class(get_workspace_stack("stack1"), "stack")
  expect_s3_class(get_workspace_stack("stack2"), "stack")

  set_workspace_title("foo")
  expect_identical(get_workspace_title(), "foo")

  set_workspace_settings(list(foo = "bar"))
  expect_identical(get_workspace_settings(), list(foo = "bar"))

  set_workspace_settings("{\"foo\": \"bar\"}")
  expect_identical(get_workspace_settings(), list(foo = "bar"))

  clear_workspace()

  expect_length(get_workspace_stacks(), 0L)
  expect_identical(get_workspace_title(), "")
  expect_identical(get_workspace_settings(), list())

  app <- serve_workspace(my_stack = new_stack(new_dataset_block))
  expect_s3_class(app, "shiny.appobj")
})

withr::local_package("shinytest2")

test_that("workspace demo works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  shiny_app_path <-
    system.file("examples/workspace/app.R", package = "blockr")
  app <- AppDriver$new(
    shiny_app_path,
    name = "workspace-app"
  )

  app$expect_values(
    input = c("myworkspace-add_stack", "myworkspace-clear_stacks"),
    export = "myworkspace-stacks"
  )
  # Add a new stack
  app$click(selector = "#myworkspace-add_stack")
  app$expect_values(
    input = c("myworkspace-add_stack", "myworkspace-clear_stacks"),
    export = "myworkspace-stacks"
  )
  app$click(selector = "#myworkspace-add_stack")
  app$expect_values(
    input = c("myworkspace-add_stack", "myworkspace-clear_stacks"),
    export = "myworkspace-stacks"
  )
  # Clear
  app$click(selector = "#myworkspace-clear_stacks")
  app$expect_values(
    input = c("myworkspace-add_stack", "myworkspace-clear_stacks"),
    export = "myworkspace-stacks"
  )
})

test_that("restore workspace works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  shiny_app_path <-
    system.file("examples/restore-workspace/app.R", package = "blockr")
  app <- AppDriver$new(
    shiny_app_path,
    name = "restore-workspace-app"
  )

  app$expect_values(
    input = c("myworkspace-add_stack", "myworkspace-clear_stacks"),
    export = "myworkspace-stacks"
  )
})
