#' process_scotgov_management
#'
#' @export
#'
process_scotgov_management <- function(sourcefile, h5filename) {

  scotMan <- read.csv(file = sourcefile) %>%
    dplyr::select(-X) %>%
    dplyr::mutate(featurecode = gsub(
      "<http://statistics.gov.scot/id/statistical-geography/", "",
      featurecode),
      featurecode = gsub(">", "", featurecode))

  # 1 -----------------------------------------------------------------------
  # Numbers of calls to NHS 111 and the coronavirus helpline
  calls.dat <- scotMan %>% dplyr::filter(grepl("Calls", variable)) %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    dplyr::mutate(variable = gsub("Calls - ", "", variable)) %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "scotland/calls",
               array = as.matrix(calls.dat),
               dimension_names = list(
                 helpline = rownames(calls.dat),
                 date = colnames(calls.dat)))

  # 2 -----------------------------------------------------------------------
  # Numbers of people in hospital and in ICU with confirmed or suspected COVID-19
  hospital.dat <- scotMan %>%
    dplyr::filter(grepl("COVID-19 patients", variable)) %>%
    dplyr::mutate(areatypename = dplyr::case_when(
      featurename == "Scotland" ~ "Country",
      nchar(featurecode) == 6 ~ "Special board",
      T ~ "NHS board"
    ))

  # Country
  hosp.country.dat <- hospital.dat %>%
    dplyr::filter(areatypename == "Country") %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "scotland/hospital",
               array = as.matrix(hosp.country.dat),
               dimension_names = list(
                 status = rownames(hosp.country.dat),
                 date = colnames(hosp.country.dat)))

  # Special health board
  hosp.special.dat <- hospital.dat %>%
    dplyr::filter(areatypename == "Special board") %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "special_health_board/hospital",
               array = as.matrix(hosp.special.dat),
               dimension_names = list(
                 status = rownames(hosp.special.dat),
                 date = colnames(hosp.special.dat)))

  # NHS health board
  hosp.nhs.dat <- hospital.dat %>%
    dplyr::filter(areatypename == "NHS board")

  hosp.nhs.total.dat <- hosp.nhs.dat %>%
    dplyr::filter(grepl("Total", variable)) %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(featurecode ~ date, value.var = "count") %>%
    tibble::column_to_rownames("featurecode")

  hosp.nhs.suspected.dat <- hosp.nhs.dat %>%
    dplyr::filter(grepl("Suspected", variable)) %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(featurecode ~ date, value.var = "count") %>%
    tibble::column_to_rownames("featurecode")

  hosp.nhs.confirmed.dat <- hosp.nhs.dat %>%
    dplyr::filter(grepl("Confirmed", variable)) %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(featurecode ~ date, value.var = "count") %>%
    tibble::column_to_rownames("featurecode")

  create_array(h5filename = h5filename,
               component = "nhs_health_board/hospital/total",
               array = as.matrix(hosp.nhs.total.dat),
               dimension_names = list(
                 `health board` = rownames(hosp.nhs.total.dat),
                 date = colnames(hosp.nhs.total.dat)))

  create_array(h5filename = h5filename,
               component = "nhs_health_board/hospital/suspected",
               array = as.matrix(hosp.nhs.suspected.dat),
               dimension_names = list(
                 `health board` = rownames(hosp.nhs.suspected.dat),
                 date = colnames(hosp.nhs.suspected.dat)))

  create_array(h5filename = h5filename,
               component = "nhs_health_board/hospital/confirmed",
               array = as.matrix(hosp.nhs.confirmed.dat),
               dimension_names = list(
                 `health board` = rownames(hosp.nhs.confirmed.dat),
                 date = colnames(hosp.nhs.confirmed.dat)))

  # 3 -----------------------------------------------------------------------
  # Numbers of ambulance attendances (total and COVID-19 suspected) and number of
  # people taken to hospital with suspected COVID-19
  ambulance.dat <- scotMan %>%
    dplyr::filter(grepl("Ambulance attendances", variable)) %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    dplyr::mutate(variable = gsub("Ambulance attendances - ", "", variable)) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "scotland/ambulance_attendances",
               array = as.matrix(ambulance.dat),
               dimension_names = list(
                 status = rownames(ambulance.dat),
                 date = colnames(ambulance.dat)))

  # 4 -----------------------------------------------------------------------
  # Number of people delayed in hospital
  discharges.dat <- scotMan %>%
    dplyr::filter(grepl("Delayed discharges", variable)) %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(1 ~ date, value.var = "count") %>%
    dplyr::select(-"1")

  create_array(h5filename = h5filename,
               component = "scotland/delayed_discharges",
               array = as.matrix(discharges.dat),
               dimension_names = list(
                 delayed = rownames(discharges.dat),
                 date = colnames(discharges.dat)))

  # 5 -----------------------------------------------------------------------
  # Numbers of people tested to date, numbers with positive and negative results,
  # and numbers of tests carried out
  testing.dat <- scotMan %>% dplyr::filter(grepl("Testing", variable)) %>%
    dplyr::mutate(areatypename = dplyr::case_when(
      featurename == "Scotland" ~ "Country",
      nchar(featurecode) == 6 ~ "Special board",
      T ~ "NHS board"
    ))

  # Country
  testing.country.dat <- testing.dat %>%
    dplyr::filter(areatypename == "Country") %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    dplyr::mutate(variable = gsub("Testing - ", "", variable)) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "scotland/testing",
               array = as.matrix(testing.country.dat),
               dimension_names = list(
                 delayed = rownames(testing.country.dat),
                 date = colnames(testing.country.dat)))

  # Special health board
  assertthat::assert_that(!"Special board" %in% testing.dat$areatypename)

  # NHS health board
  testing.area.dat <- testing.dat %>%
    dplyr::filter(areatypename == "NHS board") %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(featurecode ~ date, value.var = "count") %>%
    tibble::column_to_rownames("featurecode")

  create_array(h5filename = h5filename,
               component = "nhs_health_board/testing",
               array = as.matrix(testing.area.dat),
               dimension_names = list(
                 delayed = rownames(testing.area.dat),
                 date = colnames(testing.area.dat)))

  # 6 -----------------------------------------------------------------------
  # Numbers of NHS workforce reporting as absent due to a range of reasons
  # related to Covid-19
  nhs.dat <- scotMan %>% dplyr::filter(grepl("NHS workforce", variable)) %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    dplyr::mutate(variable = gsub("NHS workforce COVID-19 absences - ", "",
                                  variable)) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "scotland/nhs_workforce_covid19_absences",
               array = as.matrix(nhs.dat),
               dimension_names = list(
                 delayed = rownames(nhs.dat),
                 date = colnames(nhs.dat)))

  # 7 -----------------------------------------------------------------------
  # Number of care homes where suspected COVID-19 has been reported to date
  carehomes.dat <- scotMan %>%
    dplyr::filter(grepl("Adult care homes", variable)) %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    dplyr::mutate(variable = gsub("Adult care homes - ", "", variable))

  # Adult care homes ratio data
  carehomes.ratio.dat <- carehomes.dat %>%
    dplyr::filter(measure == "Ratio") %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "scotland/adult_care_homes_ratio",
               array = as.matrix(carehomes.ratio.dat),
               dimension_names = list(
                 delayed = rownames(carehomes.ratio.dat),
                 date = colnames(carehomes.ratio.dat)))

  # Adult care homes count data
  carehomes.count.dat <- carehomes.dat %>%
    dplyr::filter(measure == "Count") %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "scotland/adult_care_homes_count",
               array = as.matrix(carehomes.count.dat),
               dimension_names = list(
                 delayed = rownames(carehomes.count.dat),
                 date = colnames(carehomes.count.dat)))

  # 8 -----------------------------------------------------------------------
  # Number of COVID-19 confirmed deaths registered to date
  deaths.dat <- scotMan %>%
    dplyr::filter(grepl("deaths registered", variable)) %>%
    dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(1 ~ date, value.var = "count") %>%
    dplyr::select(-"1")

  create_array(h5filename = h5filename,
               component = "scotland/deaths",
               array = as.matrix(deaths.dat),
               dimension_names = list(
                 delayed = rownames(deaths.dat),
                 date = colnames(deaths.dat)))

}











