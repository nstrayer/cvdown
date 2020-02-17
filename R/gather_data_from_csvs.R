#' Gather CV data from CSVs
#'
#' Want to go old-school and just use plain CSVs (it _is_ simpler that way)?
#' This function loads the desired data into the proper format.
#'
#' @param positions_loc Location of positions csv
#' @param skills_loc Location of skills csv
#' @param text_blocks_loc Location of text blocks csv
#' @param contact_info_loc location of contact info CSV
#'
#' @return
#' @export
#'
#' @examples
#'
#' # Function to get sample csvs from package data store.
#' sample_data <- function(name) system.file(paste0('sample_csvs/', name), package = "cvdown")
#'
#' # Run function on these sample datasets
#' gather_data_from_csvs(positions_loc = sample_data('positions.csv'),
#'                       skills_loc = sample_data('language_skills.csv'),
#'                       text_blocks_loc = sample_data('text_blocks.csv'),
#'                       contact_info_loc = sample_data('contact_info.csv'))
#'
gather_data_from_csvs <- function(positions_loc = NULL,
                                  skills_loc = NULL,
                                  text_blocks_loc = NULL,
                                  contact_info_loc = NULL){


  has_loc <- function(loc) !is.null(loc)

  data <- list()

  if(has_loc(positions_loc)){
    data$positions <- readr::read_csv(positions_loc)
  }

  if(has_loc(skills_loc)){
    data$skills <- readr::read_csv(skills_loc)
  }

  if(has_loc(text_blocks_loc)){
    data$text_blocks <- readr::read_csv(text_blocks_loc)
  }

  if(has_loc(contact_info_loc)){
    data$contact_info <- readr::read_csv(contact_info_loc)
  }

  data
}

