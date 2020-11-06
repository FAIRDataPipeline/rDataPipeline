context("Test read_distribution")

#set paths and filename for re-usability
path <- "data-raw"
filename <- "test_distribution.toml"

rel_file_path <- paste0(path, "/", filename)

# create a distribution
name <- "latency"
distribution <- "gamma"
parameters <- list(shape = 2.0, scale = 3.0)

dist <- list(name = "latency",
             distribution = "gamma",
             parameters = list(shape = 2.0, scale = 3.0))

create_distribution(filename = filename,
                    path = path,
                    distribution = dist)

# Create a comparable list (distribution) dynamically
comp_dist <- list()
comp_dist[[name]] <- list(
  distribution = distribution)

comp_dist[[name]] <- c(comp_dist[[name]], sort(unlist(parameters), decreasing = TRUE))
comp_dist[[name]] <- c(comp_dist[[name]], type = "distribution")

# create an estimate for failure test
test_estimate_name <- "test_estimate_1.toml"
test_estimate_path <- paste0(path, "/", test_estimate_name)

create_estimate(filename = test_estimate_path,
                parameters = list(asymptomatic_period = 192.0))

#create a csv for failure test
csv_name <- "test.csv"
csv_path <- paste0(path, "/", csv_name)
write.csv(dist, csv_path)


test_that("read distribution works with toml files", {
  expect_true(is.list(read_distribution(rel_file_path)))
  expect_equivalent(read_distribution(rel_file_path), comp_dist)
})

##################################################################
##          to do add file checks to read_distribution          ##
##################################################################

test_that("read_distribution fails correctly", {
  #This needs fixing it should not warn and error:
  expect_error(suppressWarnings(read_distribution("unknown_file")))
  expect_error(read_distribution(test_estimate_path))
  expect_error(read_distribution(csv_path))
})

unlink(path, recursive = TRUE)
