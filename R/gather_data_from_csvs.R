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
gather_data_from_csvs <- function(positions_loc = NULL,
                                  skills_loc = NULL,
                                  text_blocks_loc = NULL,
                                  contact_info_loc = NULL){


  has_loc <- function(loc) !is.null(loc)

  data <- list()

  if(has_loc(positions_loc)){
    data$positions <- read_csv(positions_loc)
  }

  if(has_loc(skills_loc)){
    data$skills <- read_csv(skills_loc)
  }

  if(has_loc(text_blocks_loc)){
    data$text_blocks <- read_csv(text_blocks_loc)
  }

  if(has_loc(contact_info_loc)){
    data$contact_info <- read_csv(contact_info_loc)
  }

  data
}

