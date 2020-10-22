context("Testing validate_token()")

correct_key <- Sys.getenv("SCRC_API_TOKEN")

key_num <- paste(rep(0:9, 4), collapse = "", sep="")
key_alpha <- paste(sample(letters, 40, TRUE), collapse = "", sep="")

key_alphanum <- paste(sample(letters, 20, TRUE), rep(0:9, 4), collapse = "", sep="")

key_non_alpha <- paste(sample(letters, 20, TRUE), "/", collapse = "", sep="")

key_non_alpha_2 <- paste(sample(letters, 38, TRUE), collapse = "", sep="") %>%
  paste0("\n")

test_that("validate token fails with invalid keys", {
  expect_error(validate_token())
  expect_error(validate_token(NULL))
  expect_error(validate_token(""))
  expect_error(validate_token(as.character(Inf)))
  expect_error(validate_token(as.character(rep(0:9, 4))))
  expect_error(validate_token(40))
  expect_error(validate_token(list()))
  expect_error(validate_token(key_non_alpha))
  expect_error(validate_token(key_non_alpha_2))
  expect_error(validate_token(key_num))
  expect_error(validate_token(key_alpha))
})

test_that("Test API TOKEN works with validate_token", {
  expect_equal(correct_key, validate_token(correct_key))
})
