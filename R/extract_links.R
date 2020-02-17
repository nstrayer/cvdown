#' Extract links from text
#'
#' Helper function that takes a block of text, extracts all markdown formatted
#' links and returns the text with all links stripped out and replaced by
#' footnote references and an array of the new links to add to `cv_printer`
#' attributes.
#'
#' @param text Text containing markdown formatted links.
#' @param cv Object of class `cv_printer` that has `links` attribute containing
#'   character vector of links. Used to determin what to start footnote
#'   reference number at.
#'
#' @return
#' @export
#'
#' @examples
extract_links <- function(text, cv){

  # Regex to locate links in text
  find_link <- regex("
  \\[   # Grab opening square bracket
  .+?   # Find smallest internal text as possible
  \\]   # Closing square bracket
  \\(   # Opening parenthesis
  .+?   # Link text, again as small as possible
  \\)   # Closing parenthesis
  ", comments = TRUE)

  links <- c()
  num_links_prev <- length(attr(cv, 'links'))
  str_extract_all(text, find_link) %>%
    pluck(1) %>%
    walk(function(link_from_text){
      title <- link_from_text %>% str_extract('\\[.+\\]') %>% str_remove_all('\\[|\\]')
      link <- link_from_text %>% str_extract('\\(.+\\)') %>% str_remove_all('\\(|\\)')

      footnote_number <- num_links_prev + length(links) + 1

      # Build replacement text
      new_text <- glue('{title}<sup>{footnote_number}</sup>')

      # Replace text
      text <<- text %>% str_replace(fixed(link_from_text), new_text)

      links <<- c(links, link)
    })

  list(
    text = text,
    links = links
  )
}
