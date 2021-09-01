context("Testing initialise()")

uid <- as.character(random_hash())
data_product1 <- file.path("real", "data", uid, "1")
dataproduct_description <- "a nice description"
coderun_description <- "Do nothing"
namespace1 <- "username"

endpoint <- Sys.getenv("FDP_endpoint")

# delete_if_empty ---------------------------------------------------------

# User written config file
config_file <- paste0("config_files/initialise/config_", uid , ".yaml")

create_config(path = config_file,
              description = coderun_description,
              input_namespace = namespace1,
              output_namespace = namespace1)

# CLI functions
fair_run(path = config_file, skip = TRUE)

# Initialise code run
config <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "config.yaml")
script <- file.path(Sys.getenv("FDP_CONFIG_DIR"), "script.sh")
handle <- initialise(config, script)

finalise(handle, delete_if_empty = TRUE)

#
# SELECT key FROM authtoken_token WHERE user_id=?

# getTwitterData = function(sql) {
#   db_conn = RSQLite::dbConnect(RSQLite::SQLite(), dbname=dbfn)
#   dtp = RSQLite::dbGetQuery(db_conn, sql)
#   RSQLite::dbDisconnect(db_conn)  # clean up
#   return(dtp)
# }
