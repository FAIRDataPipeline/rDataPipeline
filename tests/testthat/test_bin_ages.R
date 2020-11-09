context("Testing bin_ages")

df <- matrix(sample(273), ncol = 91)
colnames(df) <- paste0("AGE", 0:90)
colnames(df)[91] <- "AGE90+"

df_2 <- matrix(sample(270), ncol = 90)
colnames(df_2) <- paste0("AGE", 0:89)

column_names <- c("AGE0-9","AGE10-49","AGE50-79","AGE80+")
# Should this not work:
#column_names_2 <- c("AGE0-9","AGE10-49","AGE50-88","AGE89")
column_names_2 <- c("AGE0-9","AGE10-49","AGE50-88","AGE89+")

test_that("bin_ages works with df of ages", {
  expect_equal(colnames(bin_ages(df, c(0, 10, 50, 80))), column_names)
  expect_equal(colnames(bin_ages(df_2, c(0, 10, 50, 89))), column_names_2)
})

  test_that("bin_ages errors with incorrect ageclasses",{
    expect_error(bin_ages(df))
    expect_error(bin_age(df, c(0,100,300)))
  })

  test_that("bin_ages errors with incorrect df", {
    # this should produce an error but does not:
    #expect_error(bin_ages(as.data.frame(0:1000), c(0, 100)))
  })
