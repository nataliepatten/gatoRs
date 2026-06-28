#' @title Used in gators_download() - Setting up local query
#'
#' @description
#' Downloading a parquet database containing recordsets that were not uploaded to GBIF by June 2026,
#' for the desired columns. This function sets up a local duckDB database to use in database searches.
#'
#' @importFrom arrow read_parquet write_parquet
#' @importFrom duckdb duckdb dbExistsTable dbConnect dbWriteTable dbDisconnect
#' @importFrom DBI dbExecute
#' @importFrom tools R_user_dir

setupduckDB <- function(){
  dbdir <- tools::R_user_dir("gatoRs", which = "data")
  if(dir.exists(dbdir) == FALSE){
    message(paste0("duckDB will be placed here: ", dbdir))
    dir.create(dbdir, recursive = TRUE, showWarnings = FALSE)
  }
  dbfile <- file.path(dbdir, "iMFGduckdb.duckdb")

  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = dbfile, read_only = FALSE )
    if(duckdb::dbExistsTable(con, "missing_view") == FALSE){
    message("No duckdb database found! Creating a database now!")
      local_parquet <- file.path(dbdir, "iMFG_06282026.parquet")
      if (!file.exists(local_parquet)) {
        counter <- 1
        df <- NULL
        url <- "https://github.com/mgaynor1/OccurrenceArchive/raw/main/iMFG_06282026.parquet"
        while (!is.data.frame(df)) {
          message(paste0("Attempting to download iDigBio archive - try #", counter, "!"))
          df <- tryCatch(arrow::read_parquet(url), error = function(e) NULL)
          counter <- counter + 1
          if (counter > 5) stop("Failed to download iDigBio archive after 5 attempts.")
        }
        message("iDigBio archive downloaded! Writing local copy!")
        arrow::write_parquet(df, local_parquet)
      } else {
        message("Reading in local copy of the iDigBio archive!")
        df <- arrow::read_parquet(local_parquet)
      }
    message("Creating duckDB now!")
    message("Installing rapidfuzz for fuzzy matching!")
        DBI::dbExecute(con, "INSTALL rapidfuzz FROM community;")
        DBI::dbExecute(con, "LOAD rapidfuzz;")
    message("Writing duckDB!")

    duckdb::dbWriteTable(con, "missing_view", df, overwrite = TRUE,
                         temporary = FALSE)


    }
  duckdb::dbDisconnect(con)
  rm(con)
  message("duckDB ready to query!")
}

