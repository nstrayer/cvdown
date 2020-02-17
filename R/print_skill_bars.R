#' Print skill bars
#'
#' Writes out the skill bars as divs with varying widths based on skill level
#'
#' @param cv object of class `cv_printer` created by \code{\link{new_cv_printer}}
#' @param out_of What is the skill value out of?
#' @param bar_color Color of filled in skill bar
#' @param bar_background Color of background (unfilled in part) of skill bar.
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
#' # Print skill bars
#' printer <- print_skill_bars(printer)
#'
#'
print_skill_bars <- function(cv, out_of = 5, bar_color = "#969696", bar_background = "#d9d9d9"){

  UseMethod("print_skill_bars")
}

#' @export
print_skill_bars.cv_printer <- function(cv,
                                        out_of = 5,
                                        bar_color = "#969696",
                                        bar_background = "#d9d9d9"){
  if(is.null(cv$skills)){
    stop("Missing skills data. Make sure when you setup your cv printer your passed data has skills data stored with list key of 'skills'.")
  }

  cv$skills %>%
    dplyr::mutate(width_percent = round(100*level/out_of)) %>%
    glue::glue_data(
      "<div class = 'skill-bar'",
      "style = \"background:linear-gradient(to right,",
      "{bar_color} {width_percent}%,",
      "{bar_background} {width_percent}% 100%)\" >",
      "{skill}",
      "</div>"
    ) %>%
    cat(sep = "")

  cv
}


