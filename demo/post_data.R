
library(httr)
library(jsonlite)

# api-endpoint
url <- "http://data.scrc.uk/api/model"
table <- "storage_locations"

key <- read.table("token.txt")
headers <- c(Authorization = paste("token", key))


# Add storage type --------------------------------------------------------
# Name of storage type, e.g. ftp, https
get_existing(table = "storage_type")
get_existing(table = "storage_type", key = "FTP")

user <- get_user("Sonia Mitchell", headers)


data <- new_storage_type(name = "new_type",
                         description = "description of type")
post_data(table = "storage_type", data = data, headers)


headers <- c(Authorization = paste("token", key))
data <- list(name = "new_type2",
             description = "description of type2")
table <- "storage_type"

  result <- httr::POST(file.path("http://data.scrc.uk/api", table, ""),
                     body =  jsonlite::toJSON(data, pretty = T, auto_unbox = T),
                     httr::content_type('application/json'),
                     httr::add_headers(.headers = headers),
                     verbose())


  result %>%
    httr::content("text") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)


result$status_code






# Add storage root --------------------------------------------------------

# Get storage type references for selection
get_existing(table = "storage_root")
get_existing(table = "storage_root", keys = "name")



# name of the storage root, e.g. "Boydorr"
name = "Boydorr"
# free text description of the storage root, e.g. "Boydorr FTP server"
description = "Boydorr FTP server"
# link to the root of the storage, e.g. "ftp://srv/ftp/scrc"
uri = "ftp://srv/ftp/scrc"
# reference to the storage type used. Please select storage_type from above
# storage_type = storage_types.value.split(": ")[-1]
data = c(name = name, description = description, uri = uri, type = storage_type)

post_data("storage_root", data)


# Add Storage Location ----------------------------------------------------


# Get responsible_person references for selection
users = get_picker("users", "full_name", "username")
print("Select responsible_person:")
users

# get storage_root references for selection
storage_roots = get_picker("storage_root", "name")
print("Select storage_root:")
storage_roots

# put data in the registry
# name of the storage_location, e.g. "Model File"
name = ""
# free text description of the storage_location, e.g. "Storage on Boydorr FTP for SCRC model"
description = ""
# path to the data from the root of the store, e.g. "models/my_model.txt"
path = ""
# free text toml, most likely left blank
toml_text = ""
# SHA1 hash of the file
sha1_hash = ""
# reference to a local store of the data, most likely left blank
local_cache_url = ""
# reference to the responsible_person. Please select responsible_person from above.
responsible_person = users.value.split(": ")[-1]
# reference to the storage_root. Please select storage_root from above.
store_root = storage_roots.value.split(": ")[-1]
data = c(name = name,
         description = description,
         path = path,
         toml_text = toml_text,
         hash = sha1_hash,
         local_cache_url = local_cache_url,
         responsible_person = responsible_person,
         store_root = store_root)
put_data("storage_location", data)



# Add Source Type ---------------------------------------------------------

# name of the source type, e.g. "Journal"
name = ""
# free text description of the source type
description = ""
data = c(name = name,
         description = description)
put_data("source_type", data)


# Add Source --------------------------------------------------------------


# Get responsible_person references for selection
users = get_picker("users", "full_name", "username")
print("Select responsible_person:")
users

#@title Run to get storage_location references for selection
storage_locations = get_picker("storage_location", "name", "path")
print("Select storage_location:")
storage_locations

#@title Run to get source_type references for selection
source_types = get_picker("source_type", "name", "path")
print("Select source_types:")
source_types

# name of the source
name = ""
# free text description of the source
description = ""
# reference to the responsible_person. Please select responsible_person from above.
responsible_person = users.value.split(": ")[-1]
# reference to the storage_location. Please select storage_location from above.
store = storage_locations.value.split(": ")[-1]
# reference to the source_type. Please select source_type from above.
source_type = source_types.value.split(": ")[-1]
data = c(name = name,
         description = description,
         responsible_person = responsible_person,
         store = store,
         source_type = source_type)
put_data("source", data)



