#' process_ukgov_eng_lookup
#'
#' @export
#'
process_ukgov_eng_lookup <- function(sourcefile) {

  OA_EW_LA <- readr::read_csv(sourcefile["OA_EW_LA"],
                              col_types = cols(.default = "c"))  %>%
    dplyr::rename(AREAcode = OA11CD, EWcode = WD19CD, EWname = WD19NM,
                  LAcode = LAD19CD, LAname = LAD19NM) %>%
    dplyr::select_if(grepl("name$|code$", colnames(.)))

  OA_LSOA_MSOA_LA <- readr::read_csv(sourcefile["OA_LSOA_MSOA_LA"],
                                     col_types = cols(.default = "c"))  %>%
    dplyr::rename(AREAcode = OA11CD, LSOAcode = LSOA11CD, LSOAname = LSOA11NM,
                  MSOAcode = MSOA11CD, MSOAname = MSOA11NM) %>%
    dplyr::select_if(grepl("name$|code$", colnames(.)))

  LSOA_CCG <- readr::read_csv(sourcefile["LSOA_CCG"],
                              col_types = cols(.default = "c"))  %>%
    dplyr::rename(LSOAcode = LSOA11CD, CCGcode = CCG19CD, CCGname = CCG19NM,
                  STPcode = STP19CD, STP19name = STP19NM) %>%
    dplyr::select_if(grepl("name$|code$", colnames(.)))

  EW_UA <- readr::read_csv(sourcefile["EW_UA"],
                           col_types = cols(.default = "c"))  %>%
    dplyr::rename(EWcode = WD19CD, UAcode = UA19CD, UAname = UA19NM) %>%
    dplyr::select_if(grepl("name$|code$", colnames(.)))

  UA_HB <- readr::read_csv(sourcefile["UA_HB"],
                           col_types = cols(.default = "c"))  %>%
    dplyr::rename(UAcode = UA19CD, LHBcode = LHB19CD, LHBname = LHB19NM) %>%
    dplyr::select_if(grepl("name$|code$", colnames(.)))

  conversion.table <- OA_EW_LA %>% left_join(.,OA_LSOA_MSOA_LA,by = "AREAcode") %>%
    left_join(.,LSOA_CCG,by = "LSOAcode") %>%
    left_join(.,EW_UA,by = "EWcode") %>%
    left_join(.,UA_HB,by = "UAcode")

  conversion.table$AREAname <- conversion.table$AREAcode
  write.csv(file.path("data-raw", "oa_conversion_table.csv"))
}
