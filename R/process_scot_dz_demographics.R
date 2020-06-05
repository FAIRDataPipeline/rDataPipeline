#' process_scot_dz_demographics
#'
#' @export
#'
process_scot_dz_demographics <- function(sourcefile, h5filename) {


  # Input parameters --------------------------------------------------------

  datazone_sf <- "data-raw/datazone_shapefile/SG_DataZone_Bdry_2011.shp"
  grp.names <- c("dz", "ur", "iz", "la", "hb", "mmw", "spc", "grid1km",
                 "grid10km")
  full.names <- c("data zone", "urban rural classification",
                  "intermediate zone", "local authority",
                  "health board", "multi member ward",
                  "scottish parliamentary constituency", "grid area",
                  "grid area")
  subgrp.names <- c("total", "1year", "5year", "10year",
                    "sg_deaths_scheme")
  age.classes <- list("total", 0:90, seq(0, 90, 5), seq(0, 90, 10),
                      c(0, 1, 15, 45, 65, 75, 85))


  # Prepare dz2grid ---------------------------------------------------------

  # Read in datazone shapefile and check for non-intersecting geometries
  datazones <- sf::st_read(datazone_sf, quiet = TRUE) %>% sf::st_make_valid()

  # Prepare grid sizes
  if(any(grepl("^grid", grp.names))) {
    gridsizes <- grp.names[grepl("^grid", grp.names)] %>%
      sapply(function(x) gsub("grid", "", x) %>% gsub("km", "", .)) %>%
      as.numeric()

    dz_subdivisions <- list()
    grid_matrix <- list()
    for(g in seq_along(gridsizes)) {
      tmp <- grid_intersection(datazones, gridsizes[g])
      tag <- paste0("grid", gridsizes[g], "km")
      dz_subdivisions[[g]] <- tmp$dz_subdivisions
      names(dz_subdivisions)[g] <- tag
      grid_matrix[[g]] <- tmp$grid_matrix
      names(grid_matrix)[g] <- tag
    }
  }

  # Prepare conversion table
  conversion.table <- readxl::read_excel(
    "data-raw/SIMD+2020v2+-+datazone+lookup.xlsx",
    sheet = 3) %>%
    dplyr::rename(DZcode = DZ,
                  URcode = URclass) %>%
    dplyr::select_if(grepl("name$|code$", colnames(.)))



  # Process raw data --------------------------------------------------------

  original.dat <- lapply(seq_along(sourcefile), function(k) {

    dataset <- sourcefile[k] %>%
      gsub("data-raw/sape-2018-", "", .) %>%
      gsub(".xlsx", "", .)

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
      dplyr::rename(DZcode = DataZone2011Code) %>%
      as.data.frame()


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

        if(grp.names[i] %in% c("dz", "ur", "iz", "la", "hb", "mmw", "spc")) {

          # Transformed data (non-grid transformed)
          tmp.dat <- dz2lower(transage.dat, grp.names[i], conversion.table)
          transarea.dat <- list(grid_pop = as.matrix(tmp.dat$data[, -1]),
                                grid_id = tmp.dat$data[, 1])
          area.names <- tmp.dat$area.names

        } else if(grepl("grid",  grp.names[i])) {

          # Transformed data (grid transformed)
          transarea.dat <- dz2grid(dat = transage.dat,
                                   datazones = datazones,
                                   dz_subdivisions = dz_subdivisions[[grp.names[i]]])

        } else {
          stop("OMG! - grpnames")
        }

        location <- file.path(grp.names[i], subgrp.names[j], dataset)

        dimension_names <- list(transarea.dat$grid_id,
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
