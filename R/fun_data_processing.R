#' Read all sheets from an Excel file
#'
#' This function reads all sheets from an Excel file and returns them as a list of data frames.
#'
#' @param filename The path to the Excel file.
#' @param tibble Logical indicating whether the output should be in the form of tibbles (default: \code{FALSE}).
#'
#' @return A list of data frames, with each element representing a sheet from the Excel file.
#'
#' @import readxl
#'
#' @examples
#' filename <- "path/to/excel/file.xlsx"
#' sheets <- read_excel_allsheets(filename)
#'
#' @export
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))

  # empty to NAs
  x <- lapply(x, function(df) replace(df, df == "", NA))

  if (!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


#' Convert from Shapefile to WKT format
#'
#' This function takes a dataset and a shapefile and performs the following steps:
#' 1. Selects the specified columns from the shapefile.
#' 2. Renames the selected column to "locationID".
#' 3. Merges the dataset and the modified shapefile based on the "locationID" column.
#' 4. Converts the merged data into a spatial data frame (sf) using the specified geometry column.
#' 5. Assigns the Coordinate Reference System (CRS) from the shapefile to the merged sf object.
#' 6. Returns the merged data with the WKT column.
#'
#' @param data The dataset to be merged with the shapefile.
#' @param shape The shapefile object.
#' @param id_data_colmn The column name in the dataset representing the location ID.
#' @param id_shape_colmn The column name in the shapefile representing the location ID.
#' @param geom_colmn The column name in the shapefile representing the geometry.
#'
#' @return A spatial data frame (sf) containing the merged data with the WKT column.
#'
#' @import dplyr
#' @importFrom sf st_as_sf st_crs
#'
#' @examples
#' data <- read.csv("data.csv")
#' shape <- sf::read_sf("shapefile.shp")
#' merged_data <- fromshapetowkt(data, shape, "locationID", "locationID", "geometry")
#'
#' @export
fromshapetowkt <- function(data, shape, id_data_colmn, id_shape_colmn, geom_colmn) {
  shape_short <- dplyr::select(shape, id_shape_colmn, geom_colmn)
  shape_short <- dplyr::rename(shape_short, "locationID" = id_shape_colmn)

  # Check if there are any matching locationIDs
  if (length(intersect(data[[id_data_colmn]], shape_short$locationID)) == 0) {
    warning("None of the location IDs in the uploaded data match with the selected ID column in the shape file.")
    return(data)
  }

  # Merge data and shape based on matching columns
  merged_data_sf <- merge(data, shape_short, by = "locationID", all.x = TRUE)
  merged_data_sf <- sf::st_as_sf(merged_data_sf, sf_column_name = geom_colmn)

  # Assign CRS from shape file to merged_data_sf
  sf::st_crs(merged_data_sf) <- sf::st_crs(shape)

  merged_data_sf <- sf::st_transform(merged_data_sf, 3035)

  # Return the merged data with the WKT column
  return(merged_data_sf)
}


#' Read global data from Excel file
#'
#' This function reads global data from an Excel file using the `read_excel_allsheets` function.
#'
#' @param inFile_DATA The input file containing the global data.
#'
#' @return The global data read from the Excel file.
#'
#' @seealso \code{\link{read_excel_allsheets}}
#'
#' @examples
#' inFile <- file.choose()
#' data <- read_data_global(inFile)
#'
#' @export
read_data_global <- function(inFile_DATA) {
  user_data <- read_excel_allsheets(inFile_DATA$datapath)
  return(user_data)
}


#' Read global data from GeoPackage file
#'
#' This function reads global data from a GeoPackage file using the `read_sf` function from the 'sf' package.
#'
#' @param inFile_GPKG The input file containing the GeoPackage data.
#'
#' @return The global data read from the GeoPackage file as a spatial data frame (sf).
#'
#' @importFrom sf read_sf
#'
#' @examples
#' inFile <- file.choose()
#' data <- read_gpkg_global(inFile)
#'
#' @export
read_gpkg_global <- function(inFile_GPKG) {
  file_path <- inFile_GPKG$datapath
  gpkg_data <- sf::read_sf(file_path)
  return(gpkg_data)
}


#' Read shapefile
#'
#' This function reads a shapefile from the specified path using the 'sf' package.
#' It renames the files in the shapefile directory to ensure consistency, and then reads the shapefile into a spatial data frame.
#'
#' @param shp_path The path to the shapefile.
#'
#' @return A spatial data frame (sf) representing the contents of the shapefile.
#'
#' @importFrom sf read_sf
#' @importFrom purrr walk2
#'
#' @examples
#' shp_path <- list(datapath = "path/to/shapefile", name = "shapefile.shp")
#' data <- read_shapefile(shp_path)
#'
#' @export
read_shapefile <- function(shp_path) {
  infiles <- shp_path$datapath
  dir <- unique(dirname(infiles))
  outfiles <- file.path(dir, shp_path$name)
  name <- strsplit(shp_path$name[1], "\\.")[[1]][1]
  purrr::walk2(infiles, outfiles, ~file.rename(.x, .y))
  x <- sf::read_sf(file.path(dir, paste0(name, ".shp")))
  return(x)
}

