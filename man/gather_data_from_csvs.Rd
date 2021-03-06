% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gather_data_from_csvs.R
\name{gather_data_from_csvs}
\alias{gather_data_from_csvs}
\title{Gather CV data from CSVs}
\usage{
gather_data_from_csvs(
  positions_loc = NULL,
  skills_loc = NULL,
  text_blocks_loc = NULL,
  contact_info_loc = NULL
)
}
\arguments{
\item{positions_loc}{Location of positions csv}

\item{skills_loc}{Location of skills csv}

\item{text_blocks_loc}{Location of text blocks csv}

\item{contact_info_loc}{location of contact info CSV}
}
\value{

}
\description{
Want to go old-school and just use plain CSVs (it _is_ simpler that way)?
This function loads the desired data into the proper format.
}
\examples{

# Function to get sample csvs from package data store.
sample_data <- function(name) system.file(paste0('sample_csvs/', name), package = "cvdown")

# Run function on these sample datasets
gather_data_from_csvs(positions_loc = sample_data('positions.csv'),
                      skills_loc = sample_data('language_skills.csv'),
                      text_blocks_loc = sample_data('text_blocks.csv'),
                      contact_info_loc = sample_data('contact_info.csv'))

}
