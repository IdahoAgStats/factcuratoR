#' Read in all codebooks and create a combined cultivar file
#'
#' Read in all controlled variable codebooks and create a combined cultivar file
#' in long format.
#' @inheritParams readin_db_init
#' @import dplyr
#' @import tidyr
#' @import readr
#' @import purrr
#' @returns A list of all \emph{*.csv} files in the controlled vocab folder, as is. Additionally creates
#' a combined cultivar file, cultivar.csv, selecting \emph{crop, date_added}, and all current
#' variety/alias names for each \emph{crop_type}.
#' All variety/alias names are gathered in a single column, linked with a \emph{db_id} number.
#' The \emph{type_db} column tracks whether the name is an alias or true variety name,
#' and the \emph{intid_db} column contains the cultivar name in lowercase with all spaces and
#' punctuation removed, used for matching.
#' @family access codebook functions
#' @export
readin_db <- function(db_folder){

  db <- readin_db_init(db_folder)

  # Bind all crop types together to return one cultivar list
  # The variety column = variety_db, which reflects both variety names and aliases
  variety <- get_variety_db(db_folder,
                            select_before = NULL,
                            select_crops = NULL) %>%
    rename(variety = variety_db)

  db[["cultivar.csv"]] <- variety

  return(db)
}

#' Read in database control variables
#'
#' Note: See read_delim if need to escape backslashes
#' @param db_folder A string path to the database controlled vocabulary folder
#' @importFrom magrittr `%>%`
#' @keywords internal
readin_db_init <- function(db_folder){
  db_files <- list.files(db_folder, pattern = ".csv")
  db <- as.list(db_files) %>%
    purrr::set_names() %>%
    map(., function(x){
      read_csv(paste(db_folder, x, sep = "/"), col_types = cols())
    })

  return(db)
}

#' List the names of the codebooks in the database
#'
#' @inheritParams readin_db
#' @family access codebook functions
#' @export
list_db_books <- function(db_folder){

  db <- readin_db(db_folder)

  db$codebooks_all_db.csv %>% select(book) %>% unique(.)
}

#' List the variables (column names) in a given database codebook
#'
#' @inheritParams readin_db
#' @param codebook_name A string denoting the book name
#' Use list_db_books() to see options
#' @param required_only A logical, FALSE returns all columns; TRUE returns required columns only
#' @param crop_types A vector containing the crop_types.
#' This is used to select the appropriate traits if codebook_name = "trial_data"
#' @family access codebook functions
#' @export
list_db_var <- function(db_folder, codebook_name, required_only = FALSE, crop_types){

  db <- readin_db(db_folder)

  if (!codebook_name %in% list_db_books(db_folder)$book){
    stop("'codebook_name' doesn't match a codebook name")
  }

  codebook <- db$codebooks_all_db.csv %>% filter(book == codebook_name)
  if (required_only){
    codebook <- codebook %>% filter(required == TRUE)
  }

  # Add traits to trial_data
  if (codebook_name == "trial_data"){
    traits_cb <- db$traits.csv

    if (!is.null(crop_types)){
      traits_cb <- traits_cb %>%
        filter(crop_type %in% crop_types)
    }

    codebook <- bind_rows(codebook, traits_cb %>% rename(variable = trait_name))
  }


  return(codebook)


}


#' Return cultivar names along with unique internal identifier (no spaces or special characters)
#'
#' Filter the cultivar list by the date that the cultivar was added.
#' The default value is "2021-01-01" because the original version of this function
#' only read in the original set of cultivars, which were added by J. Piaskowski
#' in 2020.
#' If select_before = NULL, then all dates will be returned.  Recommended usage
#' for curation is to use the current date, so that the curation is reproducible
#' even as the cultivar list is being continuously updated.
#'
#' Cultivar names are stored by crop_type, this function will return all the requested
#' crop_types.
#' @inheritParams readin_db
#' @param select_before A string in the format of Ymd.  The function returns
#' cultivars that were added to the datebase before this specified date.
#' @param select_crops A regular expression of crops separated by |. Note that this regex
#' will filter on the \emph{crop} column rather than crop_type. For example, there
#' are Triticale entries in the cultivar_wheat file, but they will be filtered out and
#' not matched if `select_crops` = "wheat".
#' @param for_matching logical, whether the result will be used for matching. Default is \code{FALSE}.
#' @import lubridate
#' @import purrr
#' @import stringr
#' @family access codebook functions
#' @export
get_variety_db <- function(db_folder,
                           select_before = Sys.Date(),
                           select_crops = NULL,
                           for_matching = FALSE){


  db <- readin_db_init(db_folder)

  variety_df1 <- db %>% keep(str_detect(names(.), "cultivar_"))

  variety_df <- imap(variety_df1, function(x, y){
    crop_type <- str_replace_all(str_extract(y, "_.*\\."), "[[:punct:]]", "")
    df <- x %>% mutate(crop_type = crop_type)
  })

  variety <- bind_rows(variety_df) %>%
    mutate(db_id = row_number())

  # Reading the cultivar files into R causes the date_added column to change format
  # Need to account for this problem
  check_date_format <- lubridate::parse_date_time(variety$date_added[1],
                                                  orders = "ymd",
                                                  quiet = TRUE)

  if (is.na(check_date_format)){
    variety <- variety %>%
      dplyr::mutate(date_added = lubridate::parse_date_time(date_added, orders = "mdy"))

  } else{
    variety <- variety %>%
      dplyr::mutate(date_added = lubridate::parse_date_time(date_added, orders = "ymd"))
  }


  if (!is.null(select_before)){
    select_before_datetime <- parse_date_time(select_before, orders = "ymd")
    variety <- variety %>% filter(date_added < select_before_datetime)
  }

  if (!is.null(select_crops)){
    variety <- variety %>% filter(str_detect(crop, regex(select_crops, ignore_case = T)))
  }

  variety_db <- variety %>%
    select(db_id, variety, alias, crop_db = crop, crop_type_db = crop_type, date_added) %>%
    gather(., type_db, variety_db, -c(db_id, crop_db, crop_type_db, date_added)) %>%
    filter(!is.na(variety_db)) %>%
    separate_rows(., variety_db, sep = ";")

  # This was the simplest way to rename
  # because this function and get.db() are recursive, so other functions
  # (e.g. match_variety_fns.R) can use this feature
  if (for_matching){
    variety_db <- variety_db %>% rename(date_added_db = date_added)
  }

  variety_db2 = variety_db %>%
    mutate(intid = gsub("[^A-Za-z0-9+]", "", variety_db)) %>% # will remove all characters not specified , which is needed to remove the \ backslash in one of the names
    mutate(intid = tolower(intid))
  return(variety_db2)
}
