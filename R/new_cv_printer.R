#' Setup CV printer object
#'
#' @param cv_data List of dataframes containging info for CV
#' @param pdf_mode Boolean of if document is being exported as pdf and thus
#'   links should be stripped for placement at end.
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
new_cv_printer <- function(cv_data, pdf_mode = TRUE){

  structure(
    cv_data,
    links = c(),
    pdf = pdf_mode,
    class = 'cv_printer'
  )
}


