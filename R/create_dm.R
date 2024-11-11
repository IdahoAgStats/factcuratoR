#' Create a data model object from the main codebook
#'
#' This function creates a data model from the codebook to show
#' the relationship between the controlled variables (e.g. where
#' the controlled vocabularies are defined)
#'
#' @param codebook_main_path A string that is a path to the main codebook
#' @importFrom dm as_dm
#' @importFrom DiagrammeR grViz
#' @export
#' @family data model functions
create_dm <- function(codebook_main_path){

  codebook <- read.csv(codebook_main_path)
  codebook_names <- unique(codebook$book)

  # Create individual dummy codebooks from the main codebook
  # because the main codebook has all the variables and dependencies
  # An alternate method would be to read all the codebooks with their data
  # (but only the variable names are needed.
  # The full set of controlled vocabularies is not needed.)
  # need to convert example to numeric if want to/ can color/label the diagram by type
  codebooks_ls <- map(codebook_names, function(x){
    df <- codebook %>%
      filter(book == 	x) %>%
      arrange(col_num, variable) %>%
      mutate(example = case_when(value_type == "string" ~ "string",
                                 value_type == "categorical" ~ "level",
                                 value_type == "integer" ~ "9",
                                 value_type == "continuous" ~ "9.9")) %>%
      select(book, variable, example) %>%
      pivot_wider(names_from = variable, values_from = example) %>%
      select(-book)
  }) %>% setNames(codebook_names)


  # Get primary_keys to add
  # If there are compound keys,
  # can change this to filter(book == values_defined_in) and use !!!syms(...)
  # but the foreign keys must be linked to these compound keys
  # (otherwise errors will arise)
  # and right now, there aren't any true compound keys
  cb_pk <- codebook %>% filter(primary_key == TRUE)
  # Get foreign keys to add
  cb_fk <- codebook %>%
    filter(!is.na(values_defined_in) & values_defined_in != "") %>%
    # Remove primary keys
    filter(book != values_defined_in)

  cb_dm <- dm::as_dm(codebooks_ls)

  # set primary keys
  for (x in 1:nrow(cb_pk)) {
    pk_temp <- cb_pk[x,]
    cb_dm <- cb_dm %>%
      dm::dm_add_pk(table = !!sym(pk_temp$book), columns = !!sym(pk_temp$variable))
  }

  # set foreign keys
  for (x in 1:nrow(cb_fk)) {
    fk_temp <- cb_fk[x,]
    cb_dm <- cb_dm %>%
      dm::dm_add_fk(table = !!sym(fk_temp$book),
                columns = !!sym(fk_temp$variable),
                ref_table = !!sym(fk_temp$values_defined_in))
  }

  dm::dm_draw(cb_dm, view_type = "all", backend = "DiagrammeR")

  # If want to have compound keys
  # for (x in unique(cb_pk$book)){
  #   pk_temp <- cb_pk %>% filter(book == x)
  #   cb_dm2 <- cb_dm2 %>%
  #     dm_add_pk(table = !!sym(x), columns = c(!!!syms(pk_temp$variable)))
  # }

}
