utils::globalVariables(c("table"))

#' Establish connection to crime3
#'
#' @return An odbc connection object
#' @export
#'
#' @examples \dontrun{
#' con <- psych4()
#' }
psych4_con <- function(){


  DBI::dbConnect(odbc::odbc(), "kosmos", database = "Psych4",
                 schema = "dbo")

}


#' Finds the table in the psych4 data
#'
#' @param table - the table you want to access
#' @param prefix - for filtering to other tables, used by DB admins
#'
#' @return a dbplyr connection object
#' @export
#'
#' @examples \dontrun{
#' psych4("V_NPR_DIA")
#' # to list all tables starting with V_ (those that have actual data for us)
#' psych4()
#' }
psych4 <- function(table, prefix = "V_%"){
  if(missing(table)) {
    odbc::dbListTables(con, table_name=prefix)
  }
  else {
    con %>%
      dplyr::tbl(dbplyr::in_schema(dbplyr::sql("dbo"), dbplyr::sql( {{ table }} )))
  }
}


