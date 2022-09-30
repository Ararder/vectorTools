


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
      "Found {variable} in{dataset}"
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
#'
#' @return a tibble of the the datafields extracted
#' @export
#'
#' @examples \dontrun{
#' df <- ukb_extract(c("20002", "41270"))
#' }
ukb_extract <- function(variables, ukb_data){

  # set default filepaths for UKB on vector
  if(missing(ukb_data)) {
    ukb_data <- list("/nfs/AGE/UKB/data/211014/r/ukb48847.tab",
                     "/nfs/AGE/UKB/data/211019/r/ukb48978.tab",
                     "/nfs/AGE/UKB/data/180524/r/ukb22140.tab")
  } else {
    print("Examining all datasets ending in '*.tab' in UKB folder")
    ukb_data <- fs::dir_ls("/nfs/AGE/UKB/data/", glob = "*.tab", recurse=TRUE)
  }


  # if inputting multiple variables, iterate over each.
  if(length( {{ variables }} ) <= 1 ){
    check_ukb( {{ variables }}, {{ ukb_data }} ) %>% dplyr::tibble()

  } else{
    purrr::map( {{ variables }} , check_ukb, ukb_data = {{ ukb_data }}) %>%
      purrr::reduce(dplyr::full_join, by = "f.eid") %>% dplyr::tibble()
  }

}

