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
    readr::read_tsv(dataset, col_select = append("f.eid", cols), show_col_types = FALSE)

  }

}












derive <- function(data, code){
  if(is.character(code)){
    column <-
      dplyr::filter(data, dplyr::
                      if_any(-1, ~(stringr::str_detect(.,code)))) %>%
      dplyr::mutate(x = 1L) %>%
      dplyr::select(1, x)

    names(column)[2] <- code

  } else {
    column <-
      dplyr::filter(data, dplyr::if_any(2:ncol(data),  ~.x %in% all_of(code))) %>%
      dplyr::mutate(x = 1L) %>%
      dplyr::select(1, x)
    names(column)[2] <- paste0("code", code)
  }

  print( paste0("For code: ", code, " found a total of ", nrow(column), " cases"))
  column %>%
    dplyr::mutate(temp = 1L) %>%
    dplyr::select(1, temp) %>%
    dplyr::tibble()
}

#' Check for one or several codes in all columns of a dataset
#'
#' @param codes codes to check for
#' @param col_name name of the column to be created
#' @param data a dataframe
#'
#' @return a tibble
#' @export
#'
#' @examples \dontrun{
#' bipolar <- check_for_code(c("F30", "F31", "bipolar", icd_data))
#' }
check_for_code <- function(codes, col_name, data){


  if(length(codes) == 1){
    derive(data, codes) %>%
      dplyr::mutate(temp = 1L) %>%
      dplyr::select(1, {{ col_name }} := temp) %>%
      tibble::tibble()

  } else {
    # Sometimes you want to merge multiple codes into "one" definition.
    output <-
      purrr::map(codes, derive, data = data) %>%
      purrr::reduce(dplyr::full_join, by = "f.eid") %>%
      dplyr::mutate(temp = 1L) %>%
      dplyr::select(1, {{ col_name }} := temp) %>%
      tibble::tibble()
    print(
      paste0("All joined together, found a total of ", nrow(output), " cases")
    )
    return(output)
  }
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
