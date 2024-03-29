% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confront_data.R
\name{confront_data}
\alias{confront_data}
\title{Wrapper for create_rules() and validate::confront()}
\usage{
confront_data(df, df_type, db_folder, blends = FALSE, crop_types = NULL)
}
\arguments{
\item{df}{A data.frame to be validated}

\item{df_type}{A string "trial_data" or "trials_metadata" denoting the type of validation
rules to generate}

\item{db_folder}{A string path to the database controlled vocabulary folder}

\item{blends}{A logical denoting whether to check for blends in the variety column.
The default is FALSE}

\item{crop_types}{A vector containing the crop_types.
This is used to select the appropriate traits if codebook_name = "trial_data"}
}
\description{
This functions summarises the results and fixes the
inability for confront() to detect NAs with grepl.
See create_rules() for more details.
}
\seealso{
Other validation functions: 
\code{\link{create_rules}()},
\code{\link{find_col_info}()},
\code{\link{validate_colnames}()}
}
\concept{validation functions}
