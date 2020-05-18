#' process_simd
#' 
#' @param simd_path path to simd data file 
#' 
process_simd <- function(simd_path) {
  # Read raw data 
  simd <- read.csv(simd_path)
  
  # Process data 
  simd %>% 
    dplyr::select(dz2011, simd2020_inc_rate) %>% 
    dplyr::rename(datazone = dz2011)
}