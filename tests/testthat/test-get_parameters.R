test_that("parse parameters works", {
  # set environment variables for input files and the TOOL to use
  Sys.setenv("PARAM_FILE" = testthat::test_path("testdata/in", "parameters.json"))
  Sys.setenv("CONF_FILE" = testthat::test_path("testdata/src", "tool.yml"))
  Sys.setenv("TOOL_RUN" = "foobar")

  # get the parameters
  params <- get_parameters()

  # test parsed parameters
  testthat::expect_length(params, 6)
  testthat::expect_equal(params$foo_int, 42)
  testthat::expect_type(params$foo_int, "integer")
  testthat::expect_type(params$foo_string, "character")
  testthat::expect_type(params$foo_float, "double")
  testthat::expect_length(params$foo_array, 5)
  testthat::expect_type(params$foo_boolean, "logical")
})
