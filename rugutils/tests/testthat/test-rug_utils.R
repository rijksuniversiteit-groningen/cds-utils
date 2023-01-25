test_that("Validate parameters works", {
  parameters <- rugutils::validate_parameters("violinparams_test.json","violin_schema.json",
                                              cds_package = "rugplot")
  # print(parameters)
  expect_type(parameters,"list")
})
