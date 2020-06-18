#' process_nrs_demographics
#'
#' @export
#'
process_nrs_demographics <- function(sourcefile, h5filename,lowest_available_geogrpahy) {

  if (lowest_available_geogrpahy=="Output_area"){
    LAG_name="OA"
  }else if (lowest_available_geogrpahy=="Datazone"){
    LAG_name="dz"
  }else {
    stop("Lowest_available_geography not reckognised")
  }
  # Input parameters --------------------------------------------------------

  lag_sf <- file.path("data-raw", "lag_shapefile", 
                           switch(LAG_name,
                                  dz="SG_DataZone_Bdry_2011.shp",
                                  OA="Output_Areas__December_2011__Boundaries_EW_BFC.shp"))
  grp.names <- switch(LAG_name, 
                      dz=c("lag", "ur", "iz", "la", "hb", "mmw", "spc", 
                           "grid1km","grid10km"),
                      OA=c("lag", "EW", "LA", "LSOA", "MSOA", "CCG", "STP", 
                           "UA","LHB","grid1km", "grid10km"))
                 
  full.names <- switch(LAG_name, 
                       dz=c("data zone", "urban rural classification",
                  "intermediate zone", "local authority",
                  "health board", "multi member ward",
                  "scottish parliamentary constituency", "grid area",
                  "grid area"),
                  OA=c("output area", "electoral ward", 
                           "local authority", "lower super output area", "mid-layer super output area", 
                           "clinical commissioning group", "sustainability and transformation partnership", 
                           "unitary authority", "local health board",
                           "grid area", "grid area"))
  subgrp.names <- c("total", "1year", "5year", "10year",
                    "sg_deaths_scheme")
  age.classes <- list("total", 0:90, seq(0, 90, 5), seq(0, 90, 10),
                      c(0, 1, 15, 45, 65, 75, 85))

  # Get shapefile if not already downloaded by user -------------------------
  if (!file.exists(lag_sf)) {
    download_source_version(dataset = switch(LAG_name, 
                                             dz="ukgov_scot_dz_shapefile",
                                             OA="ukgov_eng_oa_shapefile"))
  }

  # Prepare dz2grid ---------------------------------------------------------

  # Read in lowest available geography shapefile and check for non-intersecting geometries
   LAG <- sf::st_read(lag_sf, quiet = TRUE) %>% sf::st_make_valid()
  LAG <- dplyr::rename(LAG, 
                       LAG=switch(LAG_name, 
                                  OA="OA11CD", 
                                  dz="DataZone"))
  
  # Prepare grid sizes
  if(any(grepl("^grid", grp.names))) {
    gridsizes <- grp.names[grepl("^grid", grp.names)] %>%
      sapply(function(x) gsub("grid", "", x) %>% gsub("km", "", .)) %>%
      as.numeric()

    lag_subdivisions <- list()
    grid_matrix <- list()
    for(g in seq_along(gridsizes)) {
      tmp <- grid_intersection(LAG, gridsizes[g])
      if(any(names(tmp$lag_subdivisions)=="OA11CD")||any(names(tmp$lag_subdivisions)=="DataZone")){
      tmp$lag_subdivisions <- dplyr::rename(tmp$lag_subdivisions,
                                            LAG=switch(LAG_name, 
                                                       OA="OA11CD", 
                                                       dz="DataZone"))
    }
      tag <- paste0("grid", gridsizes[g], "km")
      lag_subdivisions[[g]] <- tmp$lag_subdivisions
      names(lag_subdivisions)[g] <- tag
      grid_matrix[[g]] <- tmp$grid_matrix
      names(grid_matrix)[g] <- tag
    }
  }

  # Prepare conversion table
  conversion.table <- if(LAG_name == "dz"){
    readxl::read_excel(
    "data-raw/SIMD+2020v2+-+datazone+lookup.xlsx",
    sheet = 3) %>%
    dplyr::rename(LAGcode = DZ,
                  URcode = URclass) %>%
    dplyr::select_if(grepl("name$|code$", colnames(.)))
}else if(LAG_name =="OA"){
  combine_englandwales_lookups()
}



  # Process raw data --------------------------------------------------------

  original.dat <- lapply(seq_along(sourcefile), function(k) {

    dataset <- sourcefile[k] %>%
      gsub(switch(LAG_name,
                  dz="data-raw/sape-2018-",
                  OA="data-raw/england_"), "", .) %>%
      gsub(switch(LAG_name,
                  dz=".xlsx",
                  OA=".csv"), "", .)
    if(LAG_name == "dz"){
    sape_tmp <- readxl::read_excel(sourcefile[k], col_names = FALSE)
    header <- readxl::read_excel(sourcefile[k], skip = 3, n_max = 2)
    header <- header %>%
      dplyr::rename_at(vars(grep("^\\...[1-3]", names(.))),
                       ~ as.character(header[2, 1:3])) %>%
      dplyr::rename(AllAges = "...4") %>%
      names()
        original.dat <- sape_tmp %>%
      # Remove first 6 rows
      .[-c(1:6),] %>%
      # Rename columns
      dplyr::rename_all(~header) %>%
      # Remove empty columns (the 5th column)
      dplyr::select_if(~sum(!is.na(.)) > 0) %>%
      # Remove blank rows
      dplyr::filter_all(any_vars(!is.na(.))) %>%
      # Remove copyright
      dplyr::filter_at(vars(dplyr::ends_with("Code")),
                       ~!grepl("Copyright", .)) %>%
      # Remove columns 2:4
      dplyr::select_at(vars(-dplyr::ends_with("Name"),
                            -AllAges)) %>%
      dplyr::mutate_at(vars(dplyr::starts_with("AGE")), as.numeric) %>%
      dplyr::rename(LAGcode = DataZone2011Code) %>%
      as.data.frame()
        
        }else if(LAG_name =="OA"){
    sape_tmp <- readr::read_csv(sourcefile[k], col_names = TRUE)
    header_new <- readr::read_csv(sourcefile[k], col_names = TRUE)[1,]
    header_new <- header_new %>% 
      names(.) %>% gsub(" ", "",., fixed=TRUE) %>% 
      gsub("Age", "AGE",., fixed=TRUE) %>% gsub("AGEd", "AGE",., fixed=TRUE) %>% 
      gsub("2011outputarea", "LAGcode",., fixed=TRUE)
    
    original.dat <- sape_tmp
    colnames(original.dat) <- header_new
    }



    # Generate data and attach to hdf5 file -----------------------------------

    for(j in seq_along(subgrp.names)) {

      # Aggregate age classes
      if(subgrp.names[j] == "1year") {
        transage.dat <- original.dat

      } else if(subgrp.names[j] == "total") {
        transage.dat <- bin_ages(original.dat, age.classes[[j]])

      } else {
        transage.dat <- bin_ages(original.dat, age.classes[[j]])
      }

      for(i in seq_along(grp.names)) {
        cat(paste0("\rProcessing ", j, "/", length(subgrp.names), ": ",
                   i, "/", length(grp.names), "..."))

        if(grp.names[i] %in% grp.names[!grepl("grid", grp.names)]) {

          # Transformed data (non-grid transformed)
          tmp.dat <- dz2lower(transage.dat, grp.names[i], conversion.table)
          transarea.dat <- list(grid_pop = as.matrix(tmp.dat$data[, -1]),
                                grid_id = tmp.dat$data[, 1])
          area.names <- tmp.dat$area.names

        } else if(grepl("grid",  grp.names[i])) {

          # Transformed data (grid transformed)
          transarea.dat <- dz2grid(dat = transage.dat,
                                   LAG = LAG,
                                   lag_subdivisions = lag_subdivisions[[grp.names[i]]])

        } else {
          stop("OMG! - grpnames")
        }

        location <- file.path(grp.names[i], subgrp.names[j], dataset)

        tmp <- unlist(transarea.dat$grid_id)
        names(tmp) <- NULL
        dimension_names <- list(tmp,
                                colnames(transarea.dat$grid_pop))
        names(dimension_names) <- c(full.names[i], "age groups")

        if(grepl("grid",  grp.names[i])) {
          create_array(h5filename = h5filename,
                       component = location,
                       array = transarea.dat$grid_pop,
                       dimension_names = dimension_names,
                       dimension_values = list(grid_matrix[[grp.names[i]]]),
                       dimension_units = list(gsub("grid", "", grp.names[i])))

        } else {
          create_array(h5filename = h5filename,
                       component = location,
                       array = transarea.dat$grid_pop,
                       dimension_names = dimension_names)
        }
      }
    }
  })

}
