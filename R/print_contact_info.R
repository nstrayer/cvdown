


print_contact_info <- function(cv){
  UseMethod("print_contact_info")
}


#' @export
print_contact_info.cv_printer <- function(cv){
  if(is.null(cv$contact_info)){
    stop("Missing contact info. Make sure when you setup your cv printer your passed data has contact info stored with list key of 'contact_info'.")
  }

  cv$contact_info %>%
    glue_data("- <i class='fa fa-{icon}'></i> {contact}") %>%
    cat(sep = "\n")

  cv
}
