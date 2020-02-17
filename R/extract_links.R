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
#'
#' text_w_links <- "Here is my text with a cool [markdown link](https://my_cool_website.com/a_page) in it. For good measure [here's another link.](http://another_cool_site.com/)"
#'
#' # Setup empty CV printer
#' printer <- new_cv_printer(list(), pdf_mode = TRUE)
#'
#' extract_results <- extract_links(text_w_links, printer)
#'
#' # Text w/ no links is here
#' extract_results$text
#'
#' # Extracted links are here
#' extract_results$links
#'
#' # No links stored yet
#' attr(printer, 'links')
#'
#' # We can then append these links to our CV so it remembers them for later footnote printing
#' printer <- printer %>% append_links(extract_results$links)
#'
#' # Links have been updated
#' attr(printer, 'links')
#'
extract_links <- function(text, cv){

  # Regex to locate links in text
  find_link <- stringr::regex("
    \\[   # Grab opening square bracket
    .+?   # Find smallest internal text as possible
    \\]   # Closing square bracket
    \\(   # Opening parenthesis
    .+?   # Link text, again as small as possible
    \\)   # Closing parenthesis
    ", comments = TRUE)

  links <- c()
  num_links_prev <- length(attr(cv, 'links'))

  found_links <- stringr::str_extract_all(text, find_link)[[1]]

  purrr::walk(
    found_links,
    function(link_from_text){
      title <- link_from_text %>% stringr::str_extract('\\[.+\\]') %>% stringr::str_remove_all('\\[|\\]')
      link <- link_from_text %>% stringr::str_extract('\\(.+\\)') %>% stringr::str_remove_all('\\(|\\)')

      footnote_number <- num_links_prev + length(links) + 1

      # Build replacement text
      new_text <- glue::glue('{title}<sup>{footnote_number}</sup>')

      # Replace text
      text <<- text %>% stringr::str_replace(stringr::fixed(link_from_text), new_text)

      links <<- c(links, link)
    })

  list(
    text = text,
    links = links
  )
}
