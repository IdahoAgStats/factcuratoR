library(dm)
library(purrr)

#codebook_path <- testthat::test_path(
#  "test_controlled_vocab/codebooks_all_db.csv")
codebook_path <- here("tests/testthat/test_controlled_vocab/codebooks_all_db.csv")
codebook <- read.csv(codebook_path)

### make a list of the codebooks
codebook_names <- unique(codebook$book)

# need to convert example to numeric if want to/ can color/label the diagram by type
codebooks_ls <- map(codebook_names, function(x){
  print(x)
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


# primary_keys to add
# can change this to filter(book == values_defined_in)
# but this causes problems if the foreign keyis not linked to a compound key
cb_pk <- codebook %>% filter(primary_key == TRUE)
# foreign keys to add
cb_fk <- codebook %>%
  filter(!is.na(values_defined_in) & values_defined_in != "") %>%
  # Remove primary keys
  filter(book != values_defined_in) %>%
  # Need this line for the reduced example codebook
  filter(values_defined_in %in% codebooks)

cb_dm <- as_dm(codebooks_ls)

# set primary keys
for(x in 1:nrow(cb_pk)) {
  pk_temp <- cb_pk[x,]
  cb_dm <- cb_dm %>%
    dm_add_pk(table = !!sym(pk_temp$book), columns = !!sym(pk_temp$variable))
}

# set foreign keys
for(x in 1:nrow(cb_fk)) {
  fk_temp <- cb_fk[x,]
  cb_dm <- cb_dm %>%
    dm_add_fk(table = !!sym(fk_temp$book),
              columns = !!sym(fk_temp$variable),
              ref_table = !!sym(fk_temp$values_defined_in))
}

dm_draw(cb_dm, view_type = "all")

# If want to have compound keys
# for (x in unique(cb_pk$book)){
#   pk_temp <- cb_pk %>% filter(book == x)
#   cb_dm2 <- cb_dm2 %>%
#     dm_add_pk(table = !!sym(x), columns = c(!!!syms(pk_temp$variable)))
# }

