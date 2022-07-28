utils::globalVariables(c("con", "name", ".data"))
#' Establish connection to crime3
#'
#' @return An odbc connection object
#' @export
#'
#' @examples
#' con <- crime3_con()
crime3_con <- function(){
  names <- dplyr::tibble(odbc::odbcListDataSources()) %>% dplyr::pull(name)
  stopifnot("KOSMOS" %in% names)

  DBI::dbConnect(odbc::odbc(), "kosmos", database = "MGRCRIME3",
                   schema = "MGRCRIME3")

}

#' Access one of the tables in crime3
#'
#' @param table Name of the table. Call crime3() without arguments to see a
#' list of all tables
#' @param prefix By default, calling crime3 without arguments lists all tables
#' starting with 'V_'*. By specifiying another prefix, other tables can be listed.
#'
#' @return a dblyr table, or character vector. To further get the data into the R
#' session, collect() needs to be called.
#' @export
#'
#' @examples \dontrun{
#' available_tables <- crime3()
#' meds2005 <- crime3("V_LMED2005")
#' }
crime3 <- function(table, prefix = "V_%"){
  if(missing(table)) {
    odbc::dbListTables(con, table_name=prefix)
  }
  else {
    con %>%
      dplyr::tbl(dbplyr::in_schema(dbplyr::sql("MGRCRIME3"), dbplyr::sql( {{ table }} )))
  }
}


get_icd_code <- function(code, specialist=TRUE) {
  if(specialist) table <- "V_PATIENT_DIA" else table <- "V_GVR_DIA"
  con %>%
    dplyr::tbl(dbplyr::in_schema(dbplyr::sql("MGRCRIME3"), dbplyr::sql(table))) %>%
    dplyr::filter(.data[["DIA"]] %like% paste0(code, "%")) %>%
    dplyr::collect()

}
#' Filter NPR or GVR for individuals with a set of icd codes
#'
#' @param code a character vector of ICD codes
#' @param specialist a boolean, TRUE to look in NPR, false to look in primary care
#'
#' @return a tibble of individuals
#' @export
#'
#' @examples \dontrun{
#' mdd <- crime_icd(c("F32", "F33"))
#' mdd_primary_care <- crime_icd(c("F32", "F33"), specialist=FALSE)
#' }
crime3_icd <- function(code, specialist=TRUE) {
  stopifnot(is.character(code), is.logical(specialist))

  purrr::map_df(code, get_icd_code, specialist = {{ specialist}})
}



# unique_diagnosis <- function(tbl,icd_digits=3) {
#   stopifnot(c("LOPNR", "DATUM", "DIA") %in% colnames(tbl))
#   stopifnot(is.numeric(tbl$LOPNR), is.character(tbl$DATUM), is.character(tbl$DIA))
#   tbl %>%
#     dplyr::select(all_of(c("LOPNR", "DATUM", "DIA"))) %>%
#     dplyr::group_by(LOPNR) %>%
#     dplyr::mutate(short_dia = stringr::str_sub(DIA, 1, {{ icd_digits }} )) %>%
#     dplyr::group_by(LOPNR, short_dia) %>%
#     dplyr::filter(DATUM == min(DATUM)) %>%
#     dplyr::select(-DIA) %>%
#     dplyr::distinct() %>%
#     tidyr::pivot_wider(names_from = short_dia, values_from = DATUM) %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(DATUM = lubridate::ymd(DATUM)) %>%
#
# }
# kk <- unique_diagnosis(mdd,4)
#
#
# kk %>%
#   mutate(first = do.call(pmin, c(select(., where(is.Date)), list(na.rm=TRUE))))
