#' Print stripped links to section
#'
#' Prints all the links that were stripped from various text as cv print object
#' was used to a single section with the link footnote identifier next to each
#' link.
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
#' # Print intro text block with links in it
#' printer <- print_text_block(printer, 'intro')
#'
#' # Print out links that were extracted
#' print_links(printer)
#'
print_links <- function(cv){

  UseMethod("print_links")
}

print_links.default <- function(cv){
  cat("print_links generic")
}

#' @export
print_links.cv_printer <- function(cv){
  # Only prints stuff if cv printer was in pdf mode.
  if(attr(cv, 'pdf')){
    cat("

Links {data-icon=link}
--------------------------------------------------------------------------------

<br>


  ")
    links <- attr(cv, 'links')

    purrr::walk2(links, 1:length(links), function(link, index){
      cat(glue::glue('{index}. {link}'), "\n")
    })
  }
}
