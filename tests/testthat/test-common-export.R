test_that("shared CSV writing preserves each analysis's missing-value format", {
  directory <- tempfile("learnn1-csv-format-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  data <- data.frame(label = c("A", NA_character_), value = c(1, NA_real_))
  network <- file.path(directory, "network.csv")
  forecasting <- file.path(directory, "forecasting.csv")

  getFromNamespace(".ln1NetWriteCsv", "jaspLearnN1")(data, network)
  getFromNamespace(".ln1ForeWriteCsv", "jaspLearnN1")(data, forecasting)

  expect_identical(readLines(network), c('"label","value"', '"A",1', ','))
  expect_identical(readLines(forecasting), c('"label","value"', '"A",1', 'NA,NA'))
  expect_equal(utils::read.csv(network, na.strings = ""), data)
  expect_equal(utils::read.csv(forecasting), data)
})
