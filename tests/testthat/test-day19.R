test_that("solve19", {

  expect_equal(
    solve19a(example_data_19()),
    19114
  )

  skip("no implementation")
  expect_equal(
    solve19b(example_data_19()),
    167409079868000
  )

})
