
print_text_block <- function(cv, label){
  UseMethod("print_text_block")
}


#' @export
print_text_block.cv_printer <- function(cv, label){

  if(is.null(cv$text_blocks)){
    stop("Missing text block data. Make sure when you setup your cv printer your passed data has text blocks stored with list key of 'text_blocks'.")
  }

  # Prints out from text_blocks spreadsheet blocks of text for the intro and asides.
  block_entry <- filter(cv$text_blocks, loc == label)

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
