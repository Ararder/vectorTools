utils::globalVariables(c("temp", "x", ":="))


#' Extract a set of fields for UKBB data on Vector
#'
#' @param variables Character vector of field names
#' @param ukb_data Path to UKBB data. If not passed will scan all dataframes in the UKBB folder
#' @param remove_withdrawn Remove participants that has withdrawn their consent?
#'
#' @return a tibble of the the datafields extracted
#' @export
#'
#' @examples \dontrun{
#' df <- ukb_extract(c("20002", "41270"))
#' }
ukb_extract <- function(variables, ukb_data, remove_withdrawn=TRUE){


  if(rlang::is_missing(ukb_data)) {
    cli::cli_alert_info("No filepath was passed in {.var dataset}")
    cli::cli_inform("Examining all datasets ending in '*.tab' in {.path /nfs/AGE/UKB/data/}")
    ukb_data <- fs::dir_ls("/nfs/AGE/UKB/data/", glob = "*.tab", recurse=TRUE)
  } else {
    cli::cli_alert_info("Attempting to check data from {.file {ukb_data}}")
    stopifnot("The file specified in ukb_data does not exist" = file.exists(ukb_data))
  }


  # if inputting multiple variables, iterate over each.
  out <-
    purrr::map(variables, \(var) check_ukb(variable = var, ukb_data = ukb_data)) |>
    purrr::reduce(dplyr::full_join, by = "f.eid") |>
    dplyr::tibble()

  if(remove_withdrawn){
    r <- purrr::map_df(fs::dir_ls("/nfs/AGE/UKB/data/withdraw"), data.table::fread) %>%
      dplyr::distinct() %>%
      dplyr::rename(f.eid = 1)
    print(glue::glue(
      "Identified a total of {nrow(r)} individuals that have withdrawn consent"
    ))
    out <- dplyr::anti_join(out, r)

  }

  out

}

check_ukb <- function(variable, ukb_data){
  # iterate over all ukb datasets
  purrr::map(ukb_data, \(data) check_dataset(dataset = data, variable = variable)) |>
    # remove empty list entries
    purrr::compact() |>
    purrr::reduce(dplyr::full_join, by = "f.eid")

}

check_dataset <- function(variable, dataset) {
  # Given a filepath and a colname string, check if the variable exists as a column

  # Can only do for one code at a time.
  stopifnot(length(variable) == 1)

  # Read in column names
  dataset_columns <- colnames(data.table::fread(dataset, nrows = 0))

  # extract overlap
  cols <- dataset_columns[stringr::str_detect(dataset_columns,variable)]

  if(rlang::is_empty(cols)) {

    cli::cli_alert("No columns matched {.field {variable}} in {.file {dataset}}")
    return(NULL)

  } else {
    cli::cli_alert_success("Found {length(cols)} columns matching {.field {variable}} in {.file {dataset}}")
    cli::cli_alert_info("Matching columns: {.field {cols}}")
    data.table::fread(dataset, select = c("f.eid", cols))

  }

}













#' Check for one or several codes in all columns of a dataset
#'
#' @param codes a vector of codes, either character or numeric.
#' @param col_name Should all individuals meeting the criteria be marked with 1 in a column?
#' @param data the UKB data, read into memory with [ukb_extract()]
#'
#' @return a [dplyr::tibble()]
#' @export
#'
#' @examples \dontrun{
#' bipolar <- check_for_code(c("F30", "F31", "bipolar", icd_data))
#' }
check_for_code <- function(codes, col_name, data){
  if(!rlang::is_missing(col_name)) {
    cli::cli_inform("{.emph {col_name}} was provided as a column name. This means that all rows with
                      {codes} will be marked with 1 in the column {.emph {col_name}}")

    cli::cli_alert_info("You can retain information on each subcode by not passing a value to {.var col_name}")
  }


  output <-
    purrr::map(codes, \(x) derive(code = x, data = data )) |>
    purrr::reduce(dplyr::full_join, by = "f.eid")


  if(!rlang::is_missing(col_name)) {
    output <- dplyr::select(output,1 ) |>
      dplyr::mutate({{ col_name }} := 1L)
  }

  output
}

derive <- function(code, data) {


  if(is.character(code)) ids <- dplyr::filter(data, dplyr::if_any(-1, \(x) stringr::str_detect(x,code)))
  if(is.numeric(code)) ids <- dplyr::filter(data, dplyr::if_any(-1,  \(x) x %in% all_of(code)))

  cli::cli_alert_success("Found {nrow(ids)} individuals with code {.emph {code}}")

  colname <- paste0("c.", code)
  dplyr::select(ids, 1) |>
    dplyr::mutate({{ colname }} := 1L)
}




scan_ukb <- function(variable, dataset) {
  # Can only do for one code at a time.
  stopifnot("Can only check one variable at a time" = length(variable) == 1)
  if(rlang::is_missing(dataset)) {
    ukb_data <- fs::dir_ls("/nfs/AGE/UKB/data/", glob = "*.tab", recurse=TRUE)
    cli::cli_alert_info("No filepath was passed in {.var dataset}")
    cli::cli_inform("Examining all datasets ending in '*.tab' in {.path /nfs/AGE/UKB/data/}")
  }

  # Read in column names
  dataset_columns <- data.table::fread(dataset, nrows = 0) |>
    colnames()

  # extract overlap
  cols <- dataset_columns[stringr::str_detect(dataset_columns,variable)]


  if(rlang::is_empty(cols)) {

    cli::cli_alert("No columns found for {variable} in {dataset}")
    return(NULL)

  } else {

    cli::cli_alert_success("Found {length(cols)} columns matching {variable} in {dataset}")
    return()

  }



}
