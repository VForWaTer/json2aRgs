test_that("parse parameters works", {
  # set environment variables for input files and the TOOL to use
  Sys.setenv("PARAM_FILE" = testthat::test_path("testdata/in", "parameters.json"))
  Sys.setenv("CONF_FILE" = testthat::test_path("testdata/src", "tool.yml"))
  Sys.setenv("TOOL_RUN" = "foobar")

  # get the parameters
  params <- get_parameters()

  # test parsed parameters
  testthat::expect_length(params, 7)
  testthat::expect_equal(params$foo_int, 42)
  testthat::expect_length(params$foo_array, 5)
  testthat::expect_equal(params$foo_matrix[3, 2], 6.1)
  testthat::expect_equal(colnames(params$foo_csv), c("A", "B", "C", "D"))
})
