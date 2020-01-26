#' A slower way to get the IDAT files if for some reason
#' the GEOmetadb.sqlite file is out of date or stale.
#'
#' @param GSMs a vector of GEO sample IDs
#' @param cachePath where the cached GEOmetadb SQLite file lives (tempdir())
#'
#' @return FTP file paths for IDATs
#' 
#' @import GEOquery
#' @export
#'
#' @examples
#' ftpGSMs("GSM3911862")

ftpGSMs <- function(GSMs = NULL, cachePath = NULL) {
  # try and get the ftp addresses for the IDATs
  if (is.null(GSMs)) stop("Need to supply GSMXXXXX IDs to download.")
  if (is.null(cachePath)) cachePath <- tempdir()
  gsms_to_download <- unlist(lapply(GSMs, function(g) {
    message("Getting the GEO ", g, ".soft file")
    geo_mat <- GEOquery::getGEO(g, destdir = cachePath)
    ftp_files <- geo_mat@header$supplementary_file
    return(ftp_files)
  }))
}