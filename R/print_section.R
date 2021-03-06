#' Print position section
#'
#' @inheritParams print_skill_bars
#' @param section_id Name of section to be printed as encoded by the column `section` in positions dataframe.
#' @param title Name to be printing at top of section. Defaults to `section_id` value.
#' @param icon Name of font-awesome icon to be used for section.
#' @param aside_text Name of label for aside text as stored in the `text_blocks` section of cv data. Leave as `NULL` to ommit.
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
#' # Print education section
#' printer <- print_section(printer, 'education')
#'
print_section <- function(cv, section_id, title = section_id, icon = "graduation-cap", aside_text = NULL){

  UseMethod("print_section")
}

#' @export
print_section.cv_printer <- function(cv, section_id, title = section_id, icon = "graduation-cap", aside_text = NULL){

  if(is.null(cv$positions)){
    stop("Missing position data. Make sure when you setup your cv printer your passed data has position data stored with list key of 'positions'.")
  }

  section_sep <- paste(rep('-',80), collapse = "")
  glue::glue("{title} {{data-icon={icon}}}",
             "{section_sep}",
             "",
             .sep = "\n") %>%
    cat("\n")


  # If the user has requested aside text for the is section, print if before starting main content
  has_aside_text <- !is.null(aside_text)
  if(has_aside_text){
    cat("\n::: aside", "\n")
    cv <- cv %>% print_text_block(aside_text)
    cat("\n:::\n")
  }


  data_for_position <- cv$positions %>%
    dplyr::filter(section == section_id)

  if(length(data_for_position) == 0){
    stop(glue::glue("No position data entry found for the section of {section_id}"))
  }

  data_for_position %>%
    dplyr::arrange(desc(end)) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    tidyr::pivot_longer(
      dplyr::starts_with('description'),
      names_to = 'description_num',
      values_to = 'description'
    ) %>%
    dplyr::filter(!is.na(description) | description_num == 'description_1') %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(
      descriptions = list(description),
      no_descriptions = is.na(dplyr::first(description))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(description_num == 'description_1') %>%
    dplyr::mutate(
      timeline = ifelse(
        is.na(start) | start == end,
        end,
        glue::glue('{end} - {start}')
      ),
      description_bullets = ifelse(
        no_descriptions,
        ' ',
        purrr::map_chr(descriptions, ~paste('-', ., collapse = '\n'))
      )
    ) %>% {
      x <- .
      if(attr(cv, 'pdf')){
        for(i in 1:nrow(x)){
          for(col in c('title', 'description_bullets')){
            # Parse text with extract links
            column_extraction <- extract_links(x[i, col], cv)
            # Reset the column itself with link stripped text
            x[i, col] <- column_extraction$text
            # Send the extracted links into the links array attribute for CV
            cv <<- cv %>% append_links(column_extraction$links)
          }
        }
      }
      x
    } %>%
    dplyr::mutate_all(~ifelse(is.na(.), 'N/A', .)) %>%
    glue::glue_data(attr(cv, 'position_template')) %>%
    cat(sep = "")

  cv
}



