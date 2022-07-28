utils::globalVariables(c("where", "all_of", "."))
#' Title
#'
#' @param tbl a tibble,
#' @param datecols name of columns that you want to find the earliest date among
#'
#' @return a tibble with a new column: first_date, which is the first date of all
#' date columns or among datecols
#' @export
#'
#' @examples \dontrun{
#' mdd <- crime3_icd(c("F32", "F33"))
#' first_dia <- crime3_first_unique_diagnosis(mdd, 3)
#' first_date(first_dia)
#' }
first_date <- function(tbl,datecols){
  if(missing(datecols)) {
    datecols <- colnames(dplyr::select(tbl, where(lubridate::is.Date)))
  } else {
    c <- purrr::map_chr(dplyr::select(tbl, all_of(datecols)), class)
    stopifnot(all("Date" == c))

  }

  tbl %>%
    dplyr::mutate(first_date = do.call(pmin, c(dplyr::select(.,all_of(datecols)), list(na.rm=TRUE))))
}
