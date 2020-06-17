#' oa2lower
#'
#' @param dat data
#' @param convert_to c(OA", "EW", "LA", "LSOA", "MSOA", "CCG", "STP", "UA","LHB")
#' @param conversion_table conversion table
#'
oatolower = function (dat, convert_to, conversion_table) 
{
  # Check that convert_to is valid
  assertthat::assert_that(toupper(convert_to) %in% (colnames(conversion_table) %>% 
                                                      .[grepl("name$", .)] %>% 
                                                      gsub("name", "", .)))
  # Check that all dat output areas are in the lookup table
  assertthat::assert_that(all(dat$OAcode %in% conversion_table$OAcode))
  regex <- paste0("OA|", toupper(convert_to))
  columns <- colnames(conversion_table) %>% .[grepl(regex, 
                                                    .)]
  target.code <- paste0(toupper(convert_to), "code")
  target.name <- paste0(toupper(convert_to), "name")
  
  subset.table <- conversion_table %>% 
    dplyr::select(dplyr::contains(columns))
  
  # Check that DZcode isn't duplicated in the conversion table
  assertthat::assert_that(!any(duplicated(subset.table$OAcode)))
  
  output <- dat %>% 
    dplyr::full_join(subset.table, by = "OAcode") %>% 
    dplyr::select(-dplyr::contains(columns %>% .[-grep(target.code,.)])) %>%
    dplyr::group_by_at(vars(dplyr::contains(target.code))) %>% 
    dplyr::summarise_all(~sum(.)) %>% 
    dplyr::arrange_at(vars(dplyr::contains(target.code)))
  
  
  reference <- subset.table %>% 
    dplyr::select_at(vars(one_of(target.code, target.name))) %>% 
    unique() %>% 
    dplyr::arrange_at(vars(dplyr::contains(target.code)))
  
  assertthat::assert_that(all(output[, target.code] == 
                                reference[,target.code]))
  
  list(data = output, 
       area.names = reference[, target.name])
}