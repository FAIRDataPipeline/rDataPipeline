#' process_scot_gov_deaths
#'
#' @export
#'
process_scot_gov_deaths <- function(sourcefile, h5filename) {

  scotDeaths <- read.csv(file = sourcefile) %>%
    dplyr::select(-X) %>%
    dplyr::mutate(featurecode = gsub(
      "<http://statistics.gov.scot/id/statistical-geography/", "",
      featurecode),
      featurecode = gsub(">", "", featurecode))

  covid_deaths <- scotDeaths %>%
    dplyr::filter(cause == "COVID-19 related") %>%
    dplyr::select(-type)

  cd_week <- covid_deaths %>%
    dplyr::filter(date != "2020") %>%
    dplyr::mutate(date = gsub("^w/c ", "", date))

  cd_total <- covid_deaths %>%
    dplyr::filter(date == "2020") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  # dataset 1 - Covid related deaths per week by NHS Health Board Area
  covid_deaths_per_week_by_nhsboard <- cd_week %>%
    dplyr::filter(areatypename == "Health Board Areas") %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(featurecode ~ date, value.var = "count") %>%
    tibble::column_to_rownames("featurecode")
  datasets <- c("covid_deaths_per_week_by_nhsboard")

  # don't include
  covid_deaths_by_nhsboard <- cd_total %>%
    dplyr::filter(areatypename == "Health Board Areas",
                  location == "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  # dataset 2 - covid_deaths_per_week_by_councilarea
  covid_deaths_per_week_by_councilarea <- cd_week %>%
    dplyr::filter(areatypename == "Council Areas") %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(featurecode ~ date, value.var = "count") %>%
    tibble::column_to_rownames("featurecode")
  datasets <- c(datasets, "covid_deaths_per_week_by_councilarea")

  # don't include
  covid_deaths_by_councilarea <- cd_total %>%
    dplyr::filter(areatypename == "Council Areas",
                  location == "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  cd_week_country <- cd_week %>%
    dplyr::filter(areatypename == "Country") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  covid_deaths_per_week_by_agegroup <- cd_week_country %>%
    dplyr::filter(age != "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  # dataset 3 - covid_deaths_per_week_by_agegroup_f
  covid_deaths_per_week_by_agegroup_f <- covid_deaths_per_week_by_agegroup %>%
    dplyr::filter(gender == "Female") %>%
    reshape2::dcast(age ~ date, value.var = "count") %>%
    tibble::column_to_rownames("age")
  datasets <- c(datasets, "covid_deaths_per_week_by_agegroup_f")

  # dataset 4 - covid_deaths_per_week_by_agegroup_m
  covid_deaths_per_week_by_agegroup_m <- covid_deaths_per_week_by_agegroup %>%
    dplyr::filter(gender == "Male") %>%
    reshape2::dcast(age ~ date, value.var = "count") %>%
    tibble::column_to_rownames("age")
  datasets <- c(datasets, "covid_deaths_per_week_by_agegroup_m")

  # dataset 5 - covid_deaths_per_week_by_agegroup_all
  covid_deaths_per_week_by_agegroup_all <- covid_deaths_per_week_by_agegroup %>%
    dplyr::filter(gender == "All") %>%
    reshape2::dcast(age ~ date, value.var = "count") %>%
    tibble::column_to_rownames("age")
  datasets <- c(datasets, "covid_deaths_per_week_by_agegroup_all")

  cd_week_allages <- cd_week_country %>%
    dplyr::filter(age == "All")

  # dataset 6 - covid_deaths_per_week_by_location
  covid_deaths_per_week_by_location <- cd_week_allages %>%
    dplyr::filter(location != "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(location ~ date, value.var = "count") %>%
    tibble::column_to_rownames("location")
  datasets <- c(datasets, "covid_deaths_per_week_by_location")

  # don't include
  covid_deaths_per_week <- cd_week_allages %>%
    dplyr::filter(location == "All")

  # don't include
  covid_deaths_year_to_date <- cd_total %>%
    dplyr::filter(areatypename == "Country") %>%
    dplyr::select_if(~ length(unique(.)) != 1)


  # All deaths --------------------------------------------------------------

  all_deaths <- scotDeaths %>%
    dplyr::filter(cause != "COVID-19 related") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  ad_week <- all_deaths %>%
    dplyr::filter(date != "2020") %>%
    dplyr::mutate(date = gsub("^w/c", "", date))

  ad_total <- all_deaths %>%
    dplyr::filter(date == "2020") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  # dataset 7 - all_deaths_per_week_by_nhsboard
  all_deaths_per_week_by_nhsboard <- ad_week %>%
    dplyr::filter(areatypename == "Health Board Areas") %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(featurecode ~ date, value.var = "count") %>%
    tibble::column_to_rownames("featurecode")
  datasets <- c(datasets, "all_deaths_per_week_by_nhsboard")

  # don't include
  all_deaths_by_nhsboard <- ad_total %>%
    dplyr::filter(areatypename == "Health Board Areas",
                  location == "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  # dataset 8 - all_deaths_per_week_by_councilarea
  all_deaths_per_week_by_councilarea <- ad_week %>%
    dplyr::filter(areatypename == "Council Areas") %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(featurecode ~ date, value.var = "count") %>%
    tibble::column_to_rownames("featurecode")
  datasets <- c(datasets, "all_deaths_per_week_by_councilarea")

  # don't include
  all_deaths_by_councilarea <- ad_total %>%
    dplyr::filter(areatypename == "Council Areas",
                  location == "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  ad_week_country <- ad_week %>%
    dplyr::filter(areatypename == "Country") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  all_deaths_per_week_by_agegroup <- ad_week_country %>%
    dplyr::filter(age != "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  # dataset 9 - all_deaths_per_week_by_agegroup_f
  all_deaths_per_week_by_agegroup_f <- all_deaths_per_week_by_agegroup %>%
    dplyr::filter(gender == "Female") %>%
    reshape2::dcast(age ~ date, value.var = "count") %>%
    tibble::column_to_rownames("age")
  datasets <- c(datasets, "all_deaths_per_week_by_agegroup_f")

  # dataset 10 - all_deaths_per_week_by_agegroup_m
  all_deaths_per_week_by_agegroup_m <- all_deaths_per_week_by_agegroup %>%
    dplyr::filter(gender == "Male") %>%
    reshape2::dcast(age ~ date, value.var = "count") %>%
    tibble::column_to_rownames("age")
  datasets <- c(datasets, "all_deaths_per_week_by_agegroup_m")

  # dataset 11 - all_deaths_per_week_by_agegroup_all
  all_deaths_per_week_by_agegroup_all <- all_deaths_per_week_by_agegroup %>%
    dplyr::filter(gender == "All") %>%
    reshape2::dcast(age ~ date, value.var = "count") %>%
    tibble::column_to_rownames("age")
  datasets <- c(datasets, "all_deaths_per_week_by_agegroup_all")

  ad_week_allages <- ad_week_country %>%
    dplyr::filter(age == "All")

  # dataset 12 - all_deaths_per_week_by_location
  all_deaths_per_week_by_location <- ad_week_allages %>%
    dplyr::filter(location != "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(location ~ date, value.var = "count") %>%
    tibble::column_to_rownames("location")
  datasets <- c(datasets, "all_deaths_per_week_by_location")

  # don't include
  all_deaths_per_week <- ad_week_allages %>%
    dplyr::filter(location == "All",
                  cause == "All causes") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  # dataset 13 - all_deaths_per_week_averaged_over_5years
  tmp <- "All causes - average of corresponding week over previous 5 years"
  all_deaths_per_week_averaged_over_5years <- ad_week_allages %>%
    dplyr::filter(location == "All",
                  cause == tmp) %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(. ~ date, value.var = "count") %>%
    dplyr::select(-`.`)
  datasets <- c(datasets, "all_deaths_per_week_averaged_over_5years")

  # don't include
  all_deaths_year_to_date <- ad_total %>%
    dplyr::filter(areatypename == "Country") %>%
    dplyr::select_if(~ length(unique(.)) != 1)



  # Deaths by location ------------------------------------------------------

  # dataset 14 - covid_deaths_by_nhsboard_and_location
  covid_deaths_by_nhsboard_and_location <- cd_total %>%
    dplyr::filter(areatypename == "Health Board Areas",
                  location != "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(featurecode ~ location, value.var = "count") %>%
    tibble::column_to_rownames("featurecode")
  datasets <- c(datasets, "covid_deaths_by_nhsboard_and_location")

  # dataset 15 - all_deaths_by_nhsboard_and_location
  all_deaths_by_nhsboard_and_location <- ad_total %>%
    dplyr::filter(areatypename == "Health Board Areas",
                  location != "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(featurecode ~ location, value.var = "count") %>%
    tibble::column_to_rownames("featurecode")
  datasets <- c(datasets, "all_deaths_by_nhsboard_and_location")

  # dataset 16 - covid_deaths_by_councilarea_and_location
  covid_deaths_by_councilarea_and_location <- cd_total %>%
    dplyr::filter(areatypename == "Council Areas",
                  location != "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(featurecode ~ location, value.var = "count") %>%
    tibble::column_to_rownames("featurecode")
  datasets <- c(datasets, "covid_deaths_by_councilarea_and_location")

  # dataset 17 - all_deaths_by_councilarea_and_location
  all_deaths_by_councilarea_and_location <- ad_total %>%
    dplyr::filter(areatypename == "Council Areas",
                  location != "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(featurecode ~ location, value.var = "count") %>%
    tibble::column_to_rownames("featurecode")
  datasets <- c(datasets, "all_deaths_by_councilarea_and_location")


  # -------------------------------------------------------------------------

  # Populate hdf5 file
  for(i in seq_along(datasets)) {
    create_array(h5filename = h5filename,
                 component = datasets[i],
                 array = as.matrix(get(datasets[i])),
                 dimension_names = list(
                   `feature names` = rownames(get(datasets[i])),
                   `week commencing` = colnames(get(datasets[i]))))
  }

}
