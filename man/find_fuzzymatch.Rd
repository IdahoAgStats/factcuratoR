% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match_variety_fuzzy_module.R
\name{find_fuzzymatch}
\alias{find_fuzzymatch}
\title{Find fuzzymatch for variety using stringdist using methods "lv" and "jw"}
\usage{
find_fuzzymatch(
  var_noexactmatch,
  data_name,
  intid_col,
  select_before = Sys.Date(),
  select_crops = NULL,
  knitroutputfolder = knitroutputfolder,
  db_folder
)
}
\arguments{
\item{var_noexactmatch}{A data.frame of the varieties without exact matches -
the data.frame should contain a column for variety and internal id (intid)}

\item{data_name}{A string denoting the name of the dataset}

\item{intid_col}{A string denoting the name of the internal id column
used to join the raw data and database}

\item{select_before}{A string in the format of Ymd.  The function returns
cultivars that were added to the datebase before this specified date.}

\item{select_crops}{A regular expression of crops separated by |. Note that this regex
will filter on the \emph{crop} column rather than crop_type. For example, there
are Triticale entries in the cultivar_wheat file, but they will be filtered out and
not matched if \code{select_crops} = "wheat".}

\item{knitroutputfolder}{A folder path to send output}

\item{db_folder}{A string path to the database controlled vocabulary folder}
}
\description{
Note: This function returns the fuzzymatches from stringdist.variety()
and then places stricter requirements on variety names that end in a
number with greater than one digit (e.g. the ending number must be
an exact match with a string of numbers in the fuzzymatch).  This requirement
is to limit the number of close matches for numeric variety names.
Because of this condition, must be careful when generating var_id from the raw names
to remove any trailing digits that have no meaning (e.g. +25\% refers to seed
increase and not to a cultivar name)
}
\seealso{
Other match variety modules: 
\code{\link{do_exactmatch}()},
\code{\link{process_fuzzymatch}()},
\code{\link{process_std_new_names}()}
}
\concept{match variety modules}
