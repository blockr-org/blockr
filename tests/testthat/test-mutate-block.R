test_that("mutate-block", {
  data <- datasets::iris
  block <- mutate_block(data, value = c(newcol = "2 * Petal.Length", newcol2 = "3 * Petal.Length"))

  expect_s3_class(block, "mutate_block")
  expect_type(block, "list")

  res <- evaluate_block(block, data)
  expect_true(all(c("newcol", "newcol2") %in% colnames(res)))
})

test_that("mutate_module_server handles input correctly", {
  shiny::testServer(
    mutate_module_server, {
      # Simulate input to the ACE Editor fields
      session$setInputs(`test-pl_1_name` = "new", `test-pl_1_val` = "2 * Sepal.Length")

      # Assuming there's a mechanism to trigger an action (e.g., a submit button)
      # You need to simulate that action here
      session$setInputs(`test-i_submit` = 1)

      # Test if the reactive value is updated correctly
      # This will depend on how your module processes these inputs
      expect_true("new" %in% names(out_dat()))
    },
    args = list(
      id = "test",
      x = mutate_block(data = datasets::iris),
      in_dat = reactive(datasets::iris)
    )
  )
})
