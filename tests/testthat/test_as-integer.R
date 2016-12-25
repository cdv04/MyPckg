context("integer")

test_that ("integer is integer",{

  expect_that(as.integer(2016),is_a("integer"))
})

