% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match_variety_names_check.R
\name{find_entries_raw_names}
\alias{find_entries_raw_names}
\title{Find entries for the cultivar names that need to be checked}
\usage{
find_entries_raw_names(df_names, data_raw, join_by)
}
\arguments{
\item{df_names}{A data.frame that contains the column of names to check}

\item{data_raw}{A data.frame that contains the raw data (all entries)}

\item{join_by}{A character string or named character string that is passed
to the 'by' parameter in left_join()}
}
\description{
Summarises how many times a cultivar name shows up in the raw
data per year
}
\seealso{
Other match variety functions: 
\code{\link{collect_final_matches}()},
\code{\link{create_intid}()}
}
\concept{match variety functions}
