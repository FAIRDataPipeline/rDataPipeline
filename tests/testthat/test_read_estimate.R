context("testing read_estimate")

#set paths and filename for re-usability
path <- "data-raw"

# create a distribution for failure tests
test_distribution_name <- "test_distribution.toml"
test_distribution_path <- paste0(path, "/", test_distribution_name)
dist <- list(name = "latency",
             distribution = "gamma",
             parameters = list(shape = 2.0, scale = 3.0))

create_distribution(filename = test_distribution_name,
                    path = path,
                    distribution = dist)

test_estimate_name <- "test_estimate_1.toml"
test_estimate_path <- paste0(path, "/", test_estimate_name)
parameters = list(asymptomatic_period = 192.0)

create_estimate(filename = test_estimate_name,
                path = path,
                parameters = parameters)

comp_estimate <- list()
comp_estimate[[names(parameters)[1]]] <- list(type = "point-estimate")
comp_estimate[[names(parameters)[1]]] <- c(comp_estimate[[names(parameters)[1]]],
                                           value = parameters[[1]][1])

#create a csv for failure test
csv_name <- "test.csv"
csv_path <- paste0(path, "/", csv_name)
write.csv(dist, csv_path)


test_that("read distribution works with toml files", {
  expect_true(is.list(read_estimate(test_estimate_path)))
  expect_equivalent(read_estimate(test_estimate_path), comp_estimate)
})

##################################################################
##          to do add file checks to read_distribution          ##
##################################################################

test_that("read_distribution fails correctly", {
  #This needs fixing it should not warn and error:
  expect_error(suppressWarnings(read_estimate("unknown_file")))
  expect_error(read_estimate(test_distribution_path))
  expect_error(read_estimate(csv_path))
})

unlink(path, recursive = TRUE)
