#' stop_server
#'
stop_server <- function() {
  system2("sh", "~/.fair/registry/scripts/stop_fair_registry")
}
