#' process_scotgov_management
#'
#' @export
#'
process_scotgov_management <- function(sourcefile, h5filename) {

  scotMan <- read.csv(file = sourcefile) %>%
    dplyr::select(-X) %>%
    dplyr::mutate(featurecode = gsub(
                    "<http://statistics.gov.scot/id/statistical-geography/",
                    "", featurecode),
                  featurecode = gsub(">", "", featurecode)) %>%
    dplyr::mutate(count = dplyr::case_when(count == "*" ~ "0",
                                           T ~ count)) %>%
    dplyr::mutate(count = as.numeric(count))

  # 1 -----------------------------------------------------------------------
  # Numbers of calls to NHS 111 and the coronavirus helpline
  calls.dat <- scotMan %>% dplyr::filter(grepl("Calls", variable)) %>%
    # dplyr::select_if(~ length(unique(.)) != 1) %>%
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
    dplyr::filter(areatypename == "Country")

  patients.in.hospital.dat <- hosp.country.dat %>%
    dplyr::filter(grepl("hospital", variable)) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "scotland/hospital",
               array = as.matrix(patients.in.hospital.dat),
               dimension_names = list(
                 status = rownames(patients.in.hospital.dat),
                 date = colnames(patients.in.hospital.dat)))

  patients.in.icu.dat <- hosp.country.dat %>%
    dplyr::filter(grepl("ICU", variable)) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "scotland/icu",
               array = as.matrix(patients.in.icu.dat),
               dimension_names = list(
                 status = rownames(patients.in.icu.dat),
                 date = colnames(patients.in.icu.dat)))

  # Special health board
  hosp.special.dat <- hospital.dat %>%
    dplyr::filter(areatypename == "Special board")

  special.patients.in.hosp.dat <- hosp.special.dat %>%
    dplyr::filter(grepl("hospital", variable)) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "special_health_board/hospital",
               array = as.matrix(special.patients.in.hosp.dat),
               dimension_names = list(
                 status = rownames(special.patients.in.hosp.dat),
                 date = colnames(special.patients.in.hosp.dat)))

  special.patients.in.icu.dat <- hosp.special.dat %>%
    dplyr::filter(grepl("ICU", variable)) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "special_health_board/icu",
               array = as.matrix(special.patients.in.icu.dat),
               dimension_names = list(
                 status = rownames(special.patients.in.icu.dat),
                 date = colnames(special.patients.in.icu.dat)))

  # NHS health board
  hosp.nhs.dat <- hospital.dat %>%
    dplyr::filter(areatypename == "NHS board")

  hosp.nhs.total.dat <- hosp.nhs.dat %>%
    dplyr::filter(grepl("Total", variable)) %>%
    # dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(featurecode ~ date, value.var = "count") %>%
    tibble::column_to_rownames("featurecode")

  create_array(h5filename = h5filename,
               component = "nhs_health_board/hospital/total",
               array = as.matrix(hosp.nhs.total.dat),
               dimension_names = list(
                 `health board` = rownames(hosp.nhs.total.dat),
                 date = colnames(hosp.nhs.total.dat)))

  hosp.nhs.suspected.dat <- hosp.nhs.dat %>%
    dplyr::filter(grepl("Suspected", variable)) %>%
    # dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(featurecode ~ date, value.var = "count") %>%
    tibble::column_to_rownames("featurecode")

  create_array(h5filename = h5filename,
               component = "nhs_health_board/hospital/suspected",
               array = as.matrix(hosp.nhs.suspected.dat),
               dimension_names = list(
                 `health board` = rownames(hosp.nhs.suspected.dat),
                 date = colnames(hosp.nhs.suspected.dat)))

  hosp.nhs.confirmed.dat <- hosp.nhs.dat %>%
    dplyr::filter(grepl("Confirmed", variable)) %>%
    # dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(featurecode ~ date, value.var = "count") %>%
    tibble::column_to_rownames("featurecode")

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
    # dplyr::select_if(~ length(unique(.)) != 1) %>%
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
    # dplyr::select_if(~ length(unique(.)) != 1) %>%
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
    dplyr::filter(areatypename == "Country")

  testing.daily.positive <- testing.country.dat %>%
    dplyr::filter(grepl("Daily people found positive", variable)) %>%
    # dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "scotland/testing/daily/people_found_positive",
               array = as.matrix(testing.daily.positive),
               dimension_names = list(
                 delayed = rownames(testing.daily.positive),
                 date = colnames(testing.daily.positive)))

  testing.cumulative <- testing.country.dat %>%
    dplyr::filter(grepl("- Cumulative$", variable)) %>%
    # dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "scotland/testing/cumulative/tests_carried_out",
               array = as.matrix(testing.cumulative),
               dimension_names = list(
                 delayed = rownames(testing.cumulative),
                 date = colnames(testing.cumulative)))

  testing.daily <- testing.country.dat %>%
    dplyr::filter(grepl("- Daily$", variable)) %>%
    # dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "scotland/testing/daily/tests_carried_out",
               array = as.matrix(testing.daily),
               dimension_names = list(
                 delayed = rownames(testing.daily),
                 date = colnames(testing.daily)))

  testing.country.cumulative <- testing.country.dat %>%
    dplyr::filter(grepl("Cumulative people tested", variable)) %>%
    # dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "scotland/testing/cumulative/people_tested",
               array = as.matrix(testing.country.cumulative),
               dimension_names = list(
                 delayed = rownames(testing.country.cumulative),
                 date = colnames(testing.country.cumulative)))


  # Special health board
  assertthat::assert_that(!"Special board" %in% testing.dat$areatypename)

  # NHS health board
  testing.area.dat <- testing.dat %>%
    dplyr::filter(areatypename == "NHS board") %>%
    # dplyr::select_if(~ length(unique(.)) != 1) %>%
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
    # dplyr::select_if(~ length(unique(.)) != 1) %>%
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
    dplyr::filter(grepl("Adult care homes", variable))

  # Adult care homes count data
  carehomes.ratio.dat <- carehomes.dat %>%
    dplyr::filter(measure == "Ratio")

  # Adult care homes ratio data
  carehomes.proportion.dat <- carehomes.ratio.dat %>%
    dplyr::filter(grepl("Proportion that", variable)) %>%
    # dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "scotland/carehomes/proportion_of_cases",
               array = as.matrix(carehomes.proportion.dat),
               dimension_names = list(
                 delayed = rownames(carehomes.proportion.dat),
                 date = colnames(carehomes.proportion.dat)))

  carehomes.absence.rate.dat <- carehomes.ratio.dat %>%
    dplyr::filter(grepl("Staff absence rate", variable)) %>%
    # dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "scotland/carehomes/staff_absence_rate",
               array = as.matrix(carehomes.absence.rate.dat),
               dimension_names = list(
                 delayed = rownames(carehomes.absence.rate.dat),
                 date = colnames(carehomes.absence.rate.dat)))

  carehomes.response.rate.dat <- carehomes.ratio.dat %>%
    dplyr::filter(grepl("Response rate", variable)) %>%
    # dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "scotland/carehomes/response_rate",
               array = as.matrix(carehomes.response.rate.dat),
               dimension_names = list(
                 delayed = rownames(carehomes.response.rate.dat),
                 date = colnames(carehomes.response.rate.dat)))

  # Adult care homes count data
  carehomes.count.dat <- carehomes.dat %>%
    dplyr::filter(measure == "Count")

  carehomes.count.cum.dat <- carehomes.count.dat %>%
    dplyr::filter(grepl("Cumulative number that", variable)) %>%
    # dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "scotland/carehomes/cumulative_number_reports",
               array = as.matrix(carehomes.count.cum.dat),
               dimension_names = list(
                 delayed = rownames(carehomes.count.cum.dat),
                 date = colnames(carehomes.count.cum.dat)))

  carehomes.cum.numdat <- carehomes.count.dat %>%
    dplyr::filter(grepl("Cumulative number of", variable)) %>%
    # dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "scotland/carehomes/cumulative_number_suspected",
               array = as.matrix(carehomes.cum.numdat),
               dimension_names = list(
                 delayed = rownames(carehomes.cum.numdat),
                 date = colnames(carehomes.cum.numdat)))

  carehomes.count.daily.dat <- carehomes.count.dat %>%
    dplyr::filter(grepl("Daily number", variable)) %>%
    # dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "scotland/carehomes/new_suspected_cases",
               array = as.matrix(carehomes.count.daily.dat),
               dimension_names = list(
                 delayed = rownames(carehomes.count.daily.dat),
                 date = colnames(carehomes.count.daily.dat)))

  carehomes.count.staff.dat <- carehomes.count.dat %>%
    dplyr::filter(grepl("Number of staff reported as absent", variable)) %>%
    # dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "scotland/carehomes/staff_reported_absent",
               array = as.matrix(carehomes.count.staff.dat),
               dimension_names = list(
                 delayed = rownames(carehomes.count.staff.dat),
                 date = colnames(carehomes.count.staff.dat)))

  carehomes.count.sus.dat <- carehomes.count.dat %>%
    dplyr::filter(grepl("Number with current suspected COVID-19 cases",
                        variable)) %>%
    # dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "scotland/carehomes/carehome_with_suspected_cases",
               array = as.matrix(carehomes.count.sus.dat),
               dimension_names = list(
                 delayed = rownames(carehomes.count.sus.dat),
                 date = colnames(carehomes.count.sus.dat)))

  carehomes.count.carehomes.return.dat <- carehomes.count.dat %>%
    dplyr::filter(grepl("Adult care homes which submitted a return",
                        variable)) %>%
    # dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "scotland/carehomes/carehomes_submitted_return",
               array = as.matrix(carehomes.count.carehomes.return.dat),
               dimension_names = list(
                 delayed = rownames(carehomes.count.carehomes.return.dat),
                 date = colnames(carehomes.count.carehomes.return.dat)))

  carehomes.count.total.staff.dat <- carehomes.count.dat %>%
    dplyr::filter(grepl("Total number of staff", variable)) %>%
    # dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(variable ~ date, value.var = "count") %>%
    tibble::column_to_rownames("variable")

  create_array(h5filename = h5filename,
               component = "scotland/carehomes/staff_submitted_return",
               array = as.matrix(carehomes.count.total.staff.dat),
               dimension_names = list(
                 delayed = rownames(carehomes.count.total.staff.dat),
                 date = colnames(carehomes.count.total.staff.dat)))


  # 8 -----------------------------------------------------------------------
  # Number of COVID-19 confirmed deaths registered to date
  deaths.dat <- scotMan %>%
    dplyr::filter(grepl("deaths registered", variable)) %>%
    # dplyr::select_if(~ length(unique(.)) != 1) %>%
    reshape2::dcast(1 ~ date, value.var = "count") %>%
    dplyr::select(-"1")

  create_array(h5filename = h5filename,
               component = "scotland/deaths",
               array = as.matrix(deaths.dat),
               dimension_names = list(
                 delayed = rownames(deaths.dat),
                 date = colnames(deaths.dat)))

}











