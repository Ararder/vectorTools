utils::globalVariables(c("temp", "x", ":="))


check_dataset <- function(variable, dataset){
  ## Given a dataset filepath and a colname string, check if the variable exists as a column,
  ## and if it exists, fread it.

  # Can only do for one code at a time.
  stopifnot(length(variable) == 1)

  # Read in column names
  dataset_columns <- data.table::fread(dataset, nrows = 0) %>% colnames()

  # extract overlap
  cols <- dataset_columns[stringr::str_detect(dataset_columns,variable)]

  if(!rlang::is_empty(cols)){

    print(glue::glue(
      "Found {length(cols)} {variable} in{dataset}"
    ))

    output <- data.table::fread(dataset, select = append("f.eid", cols))
    print(glue::glue(
      "Found a total of {ncol(output)-1} columns using {variable} from:
      {dataset}"
    ))
    output
  } else {
    print(glue::glue(
      "Found no columns in
      {dataset}
      "))
    return(NULL)
  }

}


check_ukb <- function(variable, ukb_data){

  purrr::map({{ ukb_data }} , variable = {{ variable }}, check_dataset) %>%
    # remove empty list entries
    purrr::compact() %>%
    purrr::reduce(dplyr::full_join, by = "f.eid")

}




#' Title
#'
#' @param variables vector or string of column names
#' @param ukb_data list of filepaths for the UKB data files
#' @param remove_withdrawn Remove participants that has withdrawn their consent?
#'
#' @return a tibble of the the datafields extracted
#' @export
#'
#' @examples \dontrun{
#' df <- ukb_extract(c("20002", "41270"))
#' }
ukb_extract <- function(variables, ukb_data=2, remove_withdrawn=TRUE){

  # set default filepaths for UKB on vector
  stopifnot(ukb_data ==1 | ukb_data==2)
  if(ukb_data == 1) {
    # most fields are here
    ukb_data <- list("/nfs/AGE/UKB/data/211014/r/ukb48847.tab",
                     "/nfs/AGE/UKB/data/211019/r/ukb48978.tab",
                     "/nfs/AGE/UKB/data/180524/r/ukb22140.tab")
  } else if(ukb_data == 2) {
    print("Examining all datasets ending in '*.tab' in UKB folder")
    ukb_data <- fs::dir_ls("/nfs/AGE/UKB/data/", glob = "*.tab", recurse=TRUE)
  }



  # if inputting multiple variables, iterate over each.
  if(length( {{ variables }} ) <= 1 ){
    out <- check_ukb( {{ variables }}, {{ ukb_data }} ) %>% dplyr::tibble()

  } else{
    out <- purrr::map( {{ variables }} , check_ukb, ukb_data = {{ ukb_data }}) %>%
      purrr::reduce(dplyr::full_join, by = "f.eid") %>% dplyr::tibble()
  }

  if(remove_withdrawn){
    r <- purrr::map_df(fs::dir_ls("/nfs/AGE/UKB/data/withdraw"), data.table::fread) %>%
      dplyr::distinct()
    out <- dplyr::anti_join(out, r)

  }

  out

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
