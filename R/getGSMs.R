#' fetch supplemental data (IDATs) for a given dataset 
#' 
#' There's probably a better way to do this, but we haven't found it. 
#' IDAT files will be dumped into the working directory, so setwd() first.
#'
#' @param GSMs        a vector of GEO sample IDs 
#' @param cachePath   where the cached GEOmetadb SQLite file lives (tempdir())
#' 
#' @return            a list of supplementary filenames, usually 2 per GSM
#' 
#' @import GEOmetadb
#' @import curl 
#'
#' @export
#'
getGSMs <- function (GSMs, cachePath = NULL,
                     metadb.path = "https://gbnci-abcc.ncifcrf.gov/geo/GEOmetadb.sqlite.gz") 
{
  if (is.null(cachePath)) 
    cachePath <- tempdir()
  cacheFile <- paste(cachePath, "GEOmetadb.sqlite", sep = "/")
  if (!file.exists(cacheFile)) {
    message("Caching GEOmetadb database...")
    getMetaDB <- function(metadb.path, destdir) {
      destfile <- "GEOmetadb.sqlite.gz"
      localfile <- paste0(destdir, "/", destfile)
      download.file(metadb.path, destfile = localfile)
      cat("Unzipping...\n")
      R.utils::gunzip(localfile, overwrite = TRUE)
      unzippedlocalfile <- gsub("[.]gz$", "", localfile)
      con <- dbConnect(SQLite(), unzippedlocalfile)
      dat <- dbGetQuery(con, "select * from metaInfo")
      dbDisconnect(con)
      cat("Metadata associate with downloaded file:\n")
      print(dat)
      return(unzippedlocalfile)
    }
    getMetaDB(metadb.path, cachePath)
  }
  con <- dbConnect(RSQLite::SQLite(), cacheFile)
  query <- paste0("SELECT gsm, supplementary_file FROM gsm WHERE gsm IN ('", 
                  paste(GSMs, collapse = "','"), "')")
  res <- dbGetQuery(con, query)
  if (length(res$supplementary_file)) {
    IDATs <- unlist(strsplit(res$supplementary_file, ";\t"))
  } else {
    message("Couldn't resolve any IDATs using GEOmetadb.sqlite.")
    message("This may be due to files not existing or the db not being up to date.")
    message("Trying via FTP...")
    IDATs <- ftpGSMs(GSMs = GSMs, cachePath = cachePath)
  }
  numfiles <- length(IDATs)
  numGSMs <- length(GSMs)
  if ((numfiles/numGSMs) < 2) 
    message("Some samples have no IDATs to fetch.")
  getIDAT <- function(IDAT) {
    fname <- basename(IDAT)
    if (fname %in% list.files()) {
      message("Found ", fname, " locally; skipping download.")
    }
    else {
      message("Downloading ", fname, " from ", IDAT, "...", 
              appendLF = FALSE)
      download.file(IDAT, fname)
      message("done.")
    }
    return(fname)
  }
  fnames <- sapply(IDATs, getIDAT)
  invisible(fnames)
}
