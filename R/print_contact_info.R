#' Print contact info
#'
#' Prints out an icon-bulleted list of contact info as supplied in `contact_info` slot of cv data.
#'
#' @inheritParams print_skill_bars
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
#' data <- gather_data_from_csvs(positions_loc = sample_data('positions.csv'),
#'                               skills_loc = sample_data('language_skills.csv'),
#'                               text_blocks_loc = sample_data('text_blocks.csv'),
#'                               contact_info_loc = sample_data('contact_info.csv'))
#'
#' # Setup CV printer
#' printer <- new_cv_printer(data, pdf_mode = TRUE)
#'
#' # Print contact info
#' printer <- print_contact_info(printer)
#'
#'
print_contact_info <- function(cv){
  UseMethod("print_contact_info")
}


#' @export
print_contact_info.cv_printer <- function(cv){
  if(is.null(cv$contact_info)){
    stop("Missing contact info. Make sure when you setup your cv printer your passed data has contact info stored with list key of 'contact_info'.")
  }

  cv$contact_info %>%
    glue::glue_data("- <i class='fa fa-{icon}'></i> {contact}") %>%
    cat(sep = "\n")

  cv
}
