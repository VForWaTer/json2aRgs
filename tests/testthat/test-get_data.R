test_that("parse data works", {
  # set environment variables for input files and the TOOL to use
  Sys.setenv("PARAM_FILE" = testthat::test_path("testdata/in", "parameters.json"))
  Sys.setenv("CONF_FILE" = testthat::test_path("testdata/src", "tool.yml"))
  Sys.setenv("TOOL_RUN" = "foobar")

  # get the data
  data <- get_data()

  # test parsed data
  testthat::expect_length(data, 2)
  testthat::expect_equal(data$foo_matrix[3, 2], 6.1)
  testthat::expect_equal(colnames(data$foo_csv), c("A", "B", "C", "D"))

  # get data paths
  data_paths <- get_data(return_data_paths = TRUE)

  # test that data paths are returned
  testthat::expect_length(data_paths, 2)
  testthat::expect_equal(data_paths$foo_matrix, "testdata/in/foo_matrix.dat")
  testthat::expect_equal(data_paths$foo_csv, "testdata/in/foo_csv.csv")
})
