
print_section <- function(cv, section_id){
  UseMethod("print_section")
}

#' @export
print_section.cv_printer <- function(cv, section_id){

  if(is.null(cv$positions)){
    stop("Missing position data. Make sure when you setup your cv printer your passed data has position data stored with list key of 'positions'.")
  }

  data_for_position <- cv$positions %>%
    filter(section == section_id)

  if(length(data_for_position) == 0){
    stop(glue::glue("No position data entry found for the section of {section_id}"))
  }

  data_for_position %>%
    arrange(desc(end)) %>%
    mutate(id = 1:n()) %>%
    pivot_longer(
      starts_with('description'),
      names_to = 'description_num',
      values_to = 'description'
    ) %>%
    filter(!is.na(description) | description_num == 'description_1') %>%
    group_by(id) %>%
    mutate(
      descriptions = list(description),
      no_descriptions = is.na(first(description))
    ) %>%
    ungroup() %>%
    filter(description_num == 'description_1') %>%
    mutate(
      timeline = ifelse(
        is.na(start) | start == end,
        end,
        glue('{end} - {start}')
      ),
      description_bullets = ifelse(
        no_descriptions,
        ' ',
        map_chr(descriptions, ~paste('-', ., collapse = '\n'))
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
            cv <- cv %>% append_links(column_extraction$links)
          }
        }
      }
      x
    } %>%
    mutate_all(~ifelse(is.na(.), 'N/A', .)) %>%
    glue_data(
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



