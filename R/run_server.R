#' run_server
#'
run_server <- function() {
  system2("sh", "~/.fair/registry/scripts/start_fair_registry")
}
