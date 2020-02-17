#' Gather CV data from a google sheet
#'
#' Loads all cv data into a single list that is taken by the new_cv_printed
#' function and used to generate your data-driven CV!
#'
#' @param positions_sheet_loc Location of the google sheet that holds your CV
#'   info. See \code\{\link{googlesheets4::read_sheet}} for more info on what
#'   form this takes.
#' @param public_sheet Info on if the sheet is public or not. Usually CV's are
#'   public so your sheet should be to. Make a sheet public by going to "share"
#'   and saying "anyone with link can view".?
#'
#' @return
#' @export
#'
#' @examples
gather_data_from_googlesheets <- function(positions_sheet_loc = NULL,
                        public_sheet = FALSE){


  # This tells google sheets to not try and authenticate. Note that this will only
  # work if your sheet has sharing set to "anyone with link can view"
  if(public_sheet){
    googlesheets4::sheets_deauth()
  } else {

    if(fs::dir_exists('.secrets')){
      options(gargle_oauth_cache = ".secrets")
    } else {
      stop("Googlesheets authentication token not found. Run googlesheets4::sheets_auth() once in the console before trying to build CV to cache a token.")
    }
  }

  # Load and return data
  list(positions    = googlesheets4::read_sheet(positions_sheet_loc, sheet = "positions"),
       skills       = googlesheets4::read_sheet(positions_sheet_loc, sheet = "language_skills"),
       text_blocks  = googlesheets4::read_sheet(positions_sheet_loc, sheet = "text_blocks"),
       contact_info = googlesheets4::read_sheet(positions_sheet_loc, sheet = "contact_info", skip = 1) )

}

