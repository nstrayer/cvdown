#' Print position section
#'
#' @inheritParams print_skill_bars
#' @param section_id Name of section to be printed as encoded by the column `section` in positions dataframe.
#'
#' @return
#' @export
#'
#' @examples
print_section <- function(cv, section_id){

  UseMethod("print_section")
}

#' @export
print_section.cv_printer <- function(cv, section_id){

  if(is.null(cv$positions)){
    stop("Missing position data. Make sure when you setup your cv printer your passed data has position data stored with list key of 'positions'.")
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
    glue::glue_data(
      "### {title}",
      "\n\n",
      "{loc}",
      "\n\n",
      "{institution}",
      "\n\n",
      "{timeline}",
      "\n\n",
      "\n\n",
      "{description_bullets}",
      "\n\n\n"
    ) %>%
    cat(sep = "")

  cv
}



