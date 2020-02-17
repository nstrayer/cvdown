#' Print text block
#'
#' Prints arbitrary block of text from the `text_blocks` section of CV data
#'
#' @inheritParams print_skill_bars
#' @param label value in the `label` column of `text_blocks` dataframe for row that holds text desired.
#'
#' @return
#' @export
#'
#' @examples
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
#' # Print a main text block
#' printer <- print_text_block(printer, 'intro')
#'
#'
print_text_block <- function(cv, label){
  UseMethod("print_text_block")
}


#' @export
print_text_block.cv_printer <- function(cv, label){

  if(is.null(cv$text_blocks)){
    stop("Missing text block data. Make sure when you setup your cv printer your passed data has text blocks stored with list key of 'text_blocks'.")
  }

  # Prints out from text_blocks spreadsheet blocks of text for the intro and asides.
  block_entry <- dplyr::filter(cv$text_blocks, loc == label)

  if(length(block_entry) == 0){
    stop(glue::glue("No text block found with label {label}"))
  }
  text_to_print <- block_entry$text

  if(attr(cv, 'pdf')){
    text_extraction <- extract_links(text_to_print, cv)

    cv <- cv %>% append_links(text_extraction$links)
    text_to_print <- text_extraction$text
  }

  cat(text_to_print)

  cv
}
