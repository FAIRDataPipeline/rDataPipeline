#' stop_server
#'
stop_server <- function() {
  system2("sh", "~/.scrc/scripts/stop_scrc_server")
}
