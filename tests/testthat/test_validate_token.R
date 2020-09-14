context("Testing validate_token")

# get the token
key <- Sys.getenv("SCRC_API_TOKEN")

test_that("validate token produces errors when invalid token supplied", {
  expect_error(validate_token(NULL))
  expect_error(validate_token(""))
  expect_error(validate_token(5))
  expect_error(validate_token())


})

test_that("validate_token works with a token", {
  expect_true(is.character(validate_token(key)))
})
