#' process_scot_dz_demographics
#'
#' @export
#'
process_scot_dz_demographics <- function(sourcefile, h5filename) {

  # Input parameters
  datazone_sf = "data-raw/datazone_shapefile/SG_DataZone_Bdry_2011.shp"
  grp.names = c("dz", "ur", "iz", "la", "hb", "mmw", "spc", "grid1km",
                "grid10km")
  subgrp.names = c("total", "1year", "5year", "10year")
  age.classes = list("total", 0:90, seq(0, 90, 5), seq(0, 90, 10))


  # Prepare hdf5 file -------------------------------------------------------

  file.h5 <- H5File$new(h5filename, mode = "w")

  grp.objects <- paste0(tolower(grp.names), ".grp")
  subgrp.objects <- paste0(grp.names, subgrp.names, ".subgrp")


  # Create groups
  for(i in seq_along(grp.names))
    assign(grp.objects[i], file.h5$create_group(grp.names[i]))

  # Create subgroups
  for(i in seq_along(grp.names)) {
    for(j in seq_along(subgrp.names)) {
      cmd <- paste0(grp.objects[i], "$create_group(subgrp.names[j])")
      subgroup_obj <- paste0(grp.names[i], ".", subgrp.names[j], ".subgrp")
      assign(subgroup_obj, eval(parse(text = cmd)))
    }
  }


  # Prepare dz2grid ---------------------------------------------------------

  # Original dataset
  # Read raw data -----------------------------------------------------------

  sape_persons <- readxl::read_excel(sourcefile, col_names = FALSE)

  # Extract header
  header <- readxl::read_excel(sourcefile, skip = 3, n_max = 2)

  # Rename first 4 columns
  header <- header %>%
    dplyr::rename_at(vars(grep("^\\...[1-3]", names(.))),
                     ~ as.character(header[2, 1:3])) %>%
    dplyr::rename(AllAges = "...4") %>%
    names()


  # Process data ------------------------------------------------------------

  original.dat <- sape_persons %>%
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

  # Read in datazone shapefile and check for non-intersecting geometries
  datazones <- sf::st_read(datazone_sf, quiet = TRUE) %>% sf::st_make_valid()

  # Prepare grid sizes
  if(any(grepl("^grid", grp.names))) {
    gridsizes <- grp.names[grepl("^grid", grp.names)] %>%
      sapply(function(x) gsub("grid", "", x) %>% gsub("km", "", .)) %>%
      as.numeric()

    for(i in gridsizes) {
      tmp <- grid_intersection(datazones, gridsizes[i])
      object.name <- paste0("dz_subdivisions_", gridsizes[i], "km")
      assign(object.name, tmp$dz_subdivisions)
      object.name <- paste0("grid_matrix_", gridsizes[i], "km")
      assign(object.name, tmp$grid_matrixs)
    }
  }



  conversion.table <- readxl::read_excel(
    "data-raw/SIMD+2020v2+-+datazone+lookup.xlsx",
    sheet = 3) %>%
    dplyr::rename(DZcode = DZ,
                  URcode = URclass) %>%
    dplyr::select_if(grepl("name$|code$", colnames(.)))

  # Attach data to hdf5 file ------------------------------------------------

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
      cat(paste0("\r", j, "/", length(subgrp.names), ": ",
                 i, "/", length(grp.names), "..."))

      if(grp.names[i] %in% c("dz", "ur", "iz", "la", "hb", "mmw", "spc")) {

        # Transformed data (non-grid transformed)
        tmp.dat <- dz2lower(transage.dat, grp.names[i], conversion.table)
        transarea.dat <- list(grid_pop = as.matrix(tmp.dat$data[, -1]),
                              grid_id = tmp.dat$data[, 1])
        area.names <- tmp.dat$area.names

      } else if(grepl("grid",  grp.names[i])) {

        if(grepl("10km$", grp.names[i])) {
          dz_subdivisions <- dz_subdivisions_10km
          grid_matrix <- grid_matrix_10km

        } else if(grepl("1km$", grp.names[i])) {
          dz_subdivisions <- dz_subdivisions_1km
          grid_matrix <- grid_matrix_1km

        } else
          stop("OMG! - grids")

        # Transformed data (grid transformed)
        transarea.dat <- dz2grid(dat = transage.dat,
                                 datazones = datazones,
                                 dz_subdivisions = dz_subdivisions)

      } else {
        stop("OMG! - grpnames")
      }

      # Attach data
      subgroup_obj <- paste0(grp.names[i], ".", subgrp.names[j], ".subgrp")

      eval(parse(text = paste0(subgroup_obj,
                               "[[\"array\"]] <- transarea.dat$grid_pop")))

      # Attach colnames
      eval(parse(text = paste0(subgroup_obj, "[[\"Dimension_2_title\"]] <- ",
                               "\"age groups\"")))
      eval(parse(text = paste0(subgroup_obj, "[[\"Dimension_2_names\"]] <- ",
                               "colnames(transarea.dat$grid_pop)")))

      # Attach rownames
      eval(parse(text = paste0(subgroup_obj,
                               "[[\"Dimension_1_title\"]] <- \"feature names\"")))
      eval(parse(text = paste0(subgroup_obj,
                               "[[\"Dimension_1_names\"]] <- transarea.dat$grid_id")))

      # For grids
      if(grepl("grid",  grp.names[i])) {
        eval(parse(text = paste0(subgroup_obj,
                                 "[[\"Dimension_1_values\"]] <- grid_matrix")))
        units <- gsub("grid", "", grp.names[i])
        eval(parse(text = paste0(subgroup_obj,
                                 "[[\"Dimension_1_units\"]] <- units")))
      }

    }
  }

  file.h5$close_all()
}
