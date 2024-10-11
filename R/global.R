# Project: enetwildIVT
#
# Author: mvarewyck (enetwildIVT adaptation by shevelp, sergio.lopez@uclm.es)
#
# globalR function: copy data to Docker volume
###############################################################################

#' Global function - run before serverFunction and uiFunction
#' @return no return value
#'
#' @export
globalFunction <- function() {
  library(enetwildIVT)

  print("global.R loading")

  ##### Docker volume
  dockerVolume <- "/opt/enetwildIVT"

  # dataDir
  dataDir <- system.file("extdata", package = "enetwildIVT")

  # Files to be included in Docker volume on first run
  volumeFiles <- c(
    "WLDM.xlsx",
    "draftEW_dict.rds",
    "metadata_dict.rds"
  )

  for (fileName in volumeFiles) {
    iFile <- list.files(dataDir, pattern = fileName, full.names = TRUE)
    destFile <- file.path(dockerVolume, basename(iFile))

    # Substitute
    if (!file.exists(destFile) || file.info(iFile)$mtime > file.info(destFile)$mtime) {
      if (file.info(iFile)$isdir == FALSE) {
        file.copy(from = iFile,
                  to = destFile, overwrite = TRUE)
        print(paste("File copied:", fileName))
      }
    }
  }

  # After copying, show the list of files in dockerVolume
  copiedFiles <- list.files(dockerVolume)
  print("Files copied to dockerVolume:")
  print(copiedFiles)
}
