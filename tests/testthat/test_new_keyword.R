context("Test new_keyword")

# get the token
key <- Sys.getenv("SCRC_API_TOKEN")

sleep_time <- 0.5

test_user <- "22"

object_id <- get_entry("object", list(updated_by = test_user))[[1]]$url

keyphrase <- paste0("keyword_Test_", format(Sys.time(), "%d%m%y%H%M%S"))

if(is.null(object_id)){
  object_id <- post_data("object",
                         list(description = keyphrase),
                         key)
}

test_that("new_keyword works as intended", {
  expect_true(is.character(new_keyword(keyphrase,
                                       object_id,
                                       key)))
})

test_that("duplicate new_keyword produces a message", {
  expect_message(expect_true(is.character(new_keyword(keyphrase,
                                       object_id,
                                       key))))
})
