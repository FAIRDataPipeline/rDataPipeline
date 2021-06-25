

# Make sure no new field types have been added to the registry

known_fields <- c("character", "integer", "field", "boolean", "datetime",
                  "numeric", "url")

run_server()

tables <- get_tables() %>% setdiff(c("users", "groups"))
fields <- lapply(tables, function(x) {
  get_fields(x) %>%
    dplyr::filter(read_only == "FALSE") %>%
    dplyr::select(data_type)
}) %>% unlist() %>% unique()

stop_server()
