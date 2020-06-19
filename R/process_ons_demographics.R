#' process_ons_demographics
#'
#' @export
#'
  
  process_ons_demographics <- function (sourcefile, h5filename, datazone_sf,
                                        grp.names, full.names, subgrp.names,
                                        age.classes){

      # Get shapefile if not already downloaded by user -------------------------
  if (!file.exists(output_area_sf)) {
    download_source_version(dataset = "ukgov_eng_oa_shapefile")
  }
  
  # Prepare dz2grid ---------------------------------------------------------
  
  # Read in datazone shapefile and check for non-intersecting geometries
  output_areas <- sf::st_read(output_area_sf, quiet = TRUE) %>% sf::st_make_valid()
  
  # Prepare grid sizes
  if (any(grepl("^grid", grp.names))) {
    gridsizes <- grp.names[grepl("^grid", grp.names)] %>% 
      sapply(function(x) gsub("grid", "", x) %>% gsub("km", "", .)) %>% 
      as.numeric()
    
    oa_subdivisions <- list()
    grid_matrix <- list()
    for (g in seq_along(gridsizes)) {
      tmp <- grid_intersection(output_areas, gridsizes[g])
      tag <- paste0("grid", gridsizes[g], "km")
      oa_subdivisions[[g]] <- tmp$subdivisions
      names(oa_subdivisions)[g] <- tag
      grid_matrix[[g]] <- tmp$grid_matrix
      names(grid_matrix)[g] <- tag
    }
  }
  # Prepare conversion table
  OA_EW_LA <- readr::read_csv("data-raw/england_lookup/output_to_ward_to_LA.csv",
                              col_types = cols(.default = "c"))  %>% 
    dplyr::rename(AREAcode = OA11CD, EWcode = WD19CD, EWname = WD19NM, 
                  LAcode = LAD19CD, LAname = LAD19NM) %>%
  dplyr::select_if(grepl("name$|code$", colnames(.)))
  OA_LSOA_MSOA_LA <- readr::read_csv("data-raw/england_lookup/output_to_LSOA_MSOA_to_LA.csv",
                                     col_types = cols(.default = "c"))  %>%
    dplyr::rename(AREAcode = OA11CD, LSOAcode = LSOA11CD, LSOAname = LSOA11NM, 
                  MSOAcode = MSOA11CD, MSOAname = MSOA11NM) %>%
    dplyr::select_if(grepl("name$|code$", colnames(.)))
  LSOA_CCG <- readr::read_csv("data-raw/england_lookup/LSOA_to_CCG.csv",
                              col_types = cols(.default = "c"))  %>% 
    dplyr::rename(LSOAcode = LSOA11CD, CCGcode = CCG19CD, CCGname = CCG19NM, 
                  STPcode = STP19CD, STP19name = STP19NM) %>%
    dplyr::select_if(grepl("name$|code$", colnames(.)))
  EW_UA <- readr::read_csv("data-raw/england_lookup/ward_to_UA_wales.csv",
                           col_types = cols(.default = "c"))  %>% 
    dplyr::rename(EWcode = WD19CD, UAcode = UA19CD, UAname = UA19NM) %>%
    dplyr::select_if(grepl("name$|code$", colnames(.)))
  UA_HB <- readr::read_csv("data-raw/england_lookup/UA_to_healthboard_wales.csv",
                           col_types = cols(.default = "c"))  %>% 
    dplyr::rename(UAcode = UA19CD, LHBcode = LHB19CD, LHBname = LHB19NM) %>%
    dplyr::select_if(grepl("name$|code$", colnames(.)))
  
  conversion.table <- OA_EW_LA %>% left_join(.,OA_LSOA_MSOA_LA,by = "AREAcode") %>%
    left_join(.,LSOA_CCG,by = "LSOAcode") %>% 
    left_join(.,EW_UA,by = "EWcode") %>%
    left_join(.,UA_HB,by = "UAcode")
  conversion.table$AREAname <- conversion.table$AREAcode
  
  # Process raw data --------------------------------------------------------
  
  original.dat <- lapply(seq_along(sourcefile), function(k) {
    
    dataset <- sourcefile[k] %>% 
      gsub("data-raw/england_", "", .) %>% 
      gsub(".csv", "", .)
    
    sape_tmp <- readr::read_csv(sourcefile[k], col_names = TRUE)
    header_new <- readr::read_csv(sourcefile[k], col_names = TRUE)[1,]
    header_new <- header_new %>% 
      names(.) %>% gsub(" ", "",., fixed=TRUE) %>% 
      gsub("Age", "AGE",., fixed=TRUE) %>% gsub("AGEd", "AGE",., fixed=TRUE) %>% 
      gsub("2011outputarea", "AREAcode",., fixed=TRUE)
    
    original.dat <- sape_tmp
    colnames(original.dat) <- header_new
    
    # Generate data and attach to hdf5 file -----------------------------------
    
    for (j in seq_along(subgrp.names)) {
      
      # Aggregate age classes
      if (subgrp.names[j] == "1year") {
        transage.dat <- original.dat
        
      } else if (subgrp.names[j] == "total") {
        transage.dat <- bin_ages(original.dat, age.classes[[j]])
        
      } else {
        transage.dat <- bin_ages(original.dat, age.classes[[j]])
        
      }
      
      for (i in seq_along(grp.names)) {
        cat(paste0("\rProcessing ", j, "/", length(subgrp.names), 
                   ": ", i, "/", length(grp.names), "..."))
        if (grp.names[i] %in% "OA"){
          tmp.dat <- list(data = transage.dat,
                          area.names = conversion.table %>%
                            rename(OAcode = AREAcode,
                                   OAname = AREAname) %>%
                            select(OAcode, OAname))
          transarea.dat <- list(grid_pop = as.matrix(tmp.dat$data[, -1]),
                                grid_id = tmp.dat$data[, 1])
          area.names <- tmp.dat$area.names
        } else if (grp.names[i] %in% 
            c("EW", "LA", "LSOA", "MSOA", "CCG", "STP", "UA","LHB")) {
          
          # Transformed data (non-grid transformed)
          tmp.dat <- convert2lower(dat = transage.dat, 
                                   convert_to = grp.names[i], 
                                   conversion_table = conversion.table)
          transarea.dat <- list(grid_pop = as.matrix(tmp.dat$data[,  -1]), 
                                grid_id = tmp.dat$data[, 1])
          area.names <- tmp.dat$area.names
          
        } else if (grepl("grid", grp.names[i])) {
          
        # Transformed data (grid transformed)
          transarea.dat <- convert2grid(dat = transage.dat,
                                   shapefile = output_areas,
                                   subdivisions = oa_subdivisions[[grp.names[i]]])
        }
        else {
          stop("OMG! - grpnames")
        }
        
        location <- file.path(grp.names[i], subgrp.names[j], dataset)
        tmp <- unlist(transarea.dat$grid_id)
        names(tmp) <- NULL
        dimension_names <- list(tmp, 
                                colnames(transarea.dat$grid_pop))
        names(dimension_names) <- c(full.names[i], "age groups")
        
        if (grepl("grid", grp.names[i])) {
          create_array(h5filename = h5filename, 
                       component = location, 
                       array = transarea.dat$grid_pop, 
                       dimension_names = dimension_names, 
                       dimension_values = list(grid_matrix[[grp.names[i]]]), 
                       dimension_units = list(gsub("grid", "", grp.names[i])))
        }else {
          create_array(h5filename = h5filename, 
                       component = location, 
                       array = transarea.dat$grid_pop, 
                       dimension_names = dimension_names)
        }
      }
    }
  })
}
