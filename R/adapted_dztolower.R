oatolower = function (dat, convert_to, conversion_table) 
{
  assertthat::assert_that(toupper(convert_to) %in% (colnames(conversion_table) %>% 
                                                      .[grepl("name$", .)] %>% gsub("name", "", .)))
  assertthat::assert_that(all(dat$OAcode %in% conversion_table$OAcode))
  regex <- paste0("OA|", toupper(convert_to))
  columns <- colnames(conversion_table) %>% .[grepl(regex, 
                                                    .)]
  target.code <- paste0(toupper(convert_to), "code")
  target.name <- paste0(toupper(convert_to), "name")
  subset.table <- conversion_table %>% dplyr::select(dplyr::contains(columns))
  assertthat::assert_that(!any(duplicated(subset.table$OAcode)))
  output <- dat %>% dplyr::full_join(subset.table, by = "OAcode") %>% 
    dplyr::select(-dplyr::contains(columns %>% .[-grep(target.code, 
                                                       .)])) %>% dplyr::group_by_at(vars(dplyr::contains(target.code))) %>% 
    dplyr::summarise_all(~sum(.)) %>% dplyr::arrange_at(vars(dplyr::contains(target.code)))
  reference <- subset.table %>% dplyr::select_at(vars(one_of(target.code, 
                                                             target.name))) %>% unique() %>% dplyr::arrange_at(vars(dplyr::contains(target.code)))
  assertthat::assert_that(all(output[, target.code] == reference[, 
                                                                 target.code]))
  list(data = output, area.names = reference[, target.name])
}