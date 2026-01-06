# Example of the cultivar matching process

``` r
library(factcuratoR)

rlang::check_installed("here")

codebook_folder <- here::here("tests/testthat/test_controlled_vocab")
example_folder <- here::here("inst/extdata/example_cultivar_matching")
knitroutputfolder = here::here(example_folder, "output")
knitroutputfolder_blends = here::here(example_folder, "output_blends")
auxiliary_files <- here::here(example_folder, "auxiliary_files")
```

## Read in raw variety names

``` r
var <- read_csv(here::here(example_folder, "example_cultivar_matching.csv"), 
                col_types = cols())

knitr::kable(var)
```

| variety | crop_type | nursery | behavior_to_test |
|:---|:---|:---|:---|
| Wildfile (W512) | wheat | HRW | main name is misspelled, so do.exactmatch matches the alias |
| wildfir | wheat | HRW | misspelling that won’t exact match |
| AAC wildfire | wheat | H_W | Exact match |
| AAC wildfire (W512) | wheat | HRW | both names will exact match and the alias will be moved to “not needed” |
| W512 | wheat | H_W | exact match with alias |
| W512 | wheat | HRW | duplicate of W512 with different nursery |
| new name 101 (alias name101) | wheat | SWW | new name with alias |
| New 101 | wheat | S_W | duplicate to (new name 1) but very different naming |
| new101 | NA | NA | same internal id as New 1 |
| new name 201 | wheat | SWW | new name |
| Name 201 | wheat | SWW | duplicate to (new name 2) but slightly different naming |
| WA008123 | wheat | NA | test fuzzymatching with numbers and leading zeros |
| melbclub | wheat | NA | test that fuzzymatching returns the correct fuzzymatches |
| wb1035cl | wheat | NA | test fuzzymatching with numbers |
| 455 | wheat | NA | test fuzzymatching with numbers |

## Cultivar matching

### Create unique internal id for names

Extraneous information in the variety name will make the matching
process more difficult - the names won’t match by exact matching and it
is less certain that the fuzzymatching functions will be able to detect
a fuzzymatch.

Some types of information that may be stored in the variety name column
that should be removed includes: parental cross, crop type, agronomic
treatments (“fill”, “sprayed”, etc).

Remove extraneous information stored in the raw variety column by
duplicating the name column and removing the extraneous information
(which can usually be accomplished by regex). The column is duplicated
because the raw variety name should be retained to match database
matches back with the data.

Additionally, if there are blends, they should be separated from the
other variety names and matched separately. Many of the variety matching
functions have the argument `is_blends`, which can be set to TRUE to
match the blends (See the section below on matching blends). The
functions will help standardize the names according to the formatting
for blends: name1;name2

Note! Once the fuzzy matching stage is started, it is important that the
`var_id` doesn’t change other it will cause problems with matching up
matches with the correct names.

``` r
names_raw <- 
  create_intid(var, 
               variety, 
               sep_aliases = "\\(", 
               crop_type, 
               nursery) %>% mutate(var_id = as.character(var_id))#, 
#> Check variety names for possible duplicates[1] "W512" "W512"
               #alias_col = alias)

knitr::kable(names_raw)
```

| variety                      | crop_type | nursery | type    | intid        | var_id |
|:-----------------------------|:----------|:--------|:--------|:-------------|:-------|
| 455                          | wheat     | NA      | variety | 455          | 1      |
| AAC wildfire                 | wheat     | H_W     | variety | aacwildfire  | 2      |
| AAC wildfire (W512)          | wheat     | HRW     | variety | aacwildfire  | 3      |
| AAC wildfire (W512)          | wheat     | HRW     | alias   | w512         | 3      |
| Name 201                     | wheat     | SWW     | variety | name201      | 4      |
| New 101                      | wheat     | S_W     | variety | new101       | 5      |
| W512                         | wheat     | H_W;HRW | variety | w512         | 6      |
| WA008123                     | wheat     | NA      | variety | wa008123     | 7      |
| Wildfile (W512)              | wheat     | HRW     | variety | wildfile     | 8      |
| Wildfile (W512)              | wheat     | HRW     | alias   | w512         | 8      |
| melbclub                     | wheat     | NA      | variety | melbclub     | 9      |
| new name 101 (alias name101) | wheat     | SWW     | variety | newname101   | 10     |
| new name 101 (alias name101) | wheat     | SWW     | alias   | aliasname101 | 10     |
| new name 201                 | wheat     | SWW     | variety | newname201   | 11     |
| new101                       | NA        | NA      | variety | new101       | 12     |
| wb1035cl                     | wheat     | NA      | variety | wb1035cl     | 13     |
| wildfir                      | wheat     | HRW     | variety | wildfir      | 14     |

### Do exact match

The first step is to exact match to the cultivar names in the codebooks

The second step is to exact match to the names in the cv_rename.csv.
cv_rename.csv contains misspellings.

``` r
results_exactmatch <- 
  do_exactmatch(db_folder = codebook_folder,
                data_intid = names_raw,
                select_before = "2021-11-22",
                match_type = "raw")
#> match: 5 nomatch: 10 check: 0 not_needed: 2

results_rename <- 
  do_exactmatch(db_folder = codebook_folder, 
                data_intid = results_exactmatch$nomatch, 
                match_type = "raw", 
                rename_df = TRUE,
                rename_df_path = here::here(codebook_folder, "cv_rename.csv"))
#> Rows: 147 Columns: 4
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (4): correct_variety_name, wrong_name, program, crop_type
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> var_id and db_id do not exist. test_multmatch and test_aliasdup not run
#> 
#> match: 2 nomatch: 8 check: 0 not_needed: 0

knitr::kable(results_exactmatch[["match"]])
```

| variety | crop_type | nursery | type | intid | var_id | db_id | crop_db | crop_type_db | date_added_db | type_db | variety_db | intid_db | is_truematch | any_match |
|:---|:---|:---|:---|:---|:---|---:|:---|:---|:---|:---|:---|:---|:---|:---|
| 455 | wheat | NA | variety | 455 | 1 | 1141 | Hard White Spring Wheat | wheat | 2021-09-15 | variety | 455 | 455 | TRUE | TRUE |
| AAC wildfire | wheat | H_W | variety | aacwildfire | 2 | 437 | Hard Red Winter Wheat | wheat | 2020-06-08 | variety | AAC Wildfire | aacwildfire | TRUE | TRUE |
| AAC wildfire (W512) | wheat | HRW | variety | aacwildfire | 3 | 437 | Hard Red Winter Wheat | wheat | 2020-06-08 | variety | AAC Wildfire | aacwildfire | TRUE | TRUE |
| W512 | wheat | H_W;HRW | variety | w512 | 6 | 437 | Hard Red Winter Wheat | wheat | 2020-06-08 | alias | W512 | w512 | TRUE | TRUE |
| Wildfile (W512) | wheat | HRW | alias | w512 | 8 | 437 | Hard Red Winter Wheat | wheat | 2020-06-08 | alias | W512 | w512 | TRUE | TRUE |

``` r
knitr::kable(results_rename[["match"]])
```

| variety | crop_type | nursery | type | intid | var_id | intid_db_db | variety_db | wrong_name | crop_type_db | type_db | intid_db | is_truematch | any_match |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| melbclub | wheat | NA | variety | melbclub | 9 | melba | Melba | melbclub | wheat | NA | melbclub | TRUE | TRUE |
| wb1035cl | wheat | NA | variety | wb1035cl | 13 | wb1035cl+ | WB-1035CL+ | WB 1035 CL | wheat | NA | wb1035cl | TRUE | TRUE |

``` r
knitr::kable(results_rename[["nomatch"]])
```

| variety | crop_type | nursery | type | intid | var_id | wrong_name |
|:---|:---|:---|:---|:---|:---|:---|
| Name 201 | wheat | SWW | variety | name201 | 4 | NA |
| New 101 | wheat | S_W | variety | new101 | 5 | NA |
| WA008123 | wheat | NA | variety | wa008123 | 7 | NA |
| new name 101 (alias name101) | wheat | SWW | variety | newname101 | 10 | NA |
| new name 101 (alias name101) | wheat | SWW | alias | aliasname101 | 10 | NA |
| new name 201 | wheat | SWW | variety | newname201 | 11 | NA |
| new101 | NA | NA | variety | new101 | 12 | NA |
| wildfir | wheat | HRW | variety | wildfir | 14 | NA |

### Do fuzzymatch

The fuzzy matching procedure, a wrapper for
[`fuzzyjoin::stringdist_join()`](https://rdrr.io/pkg/fuzzyjoin/man/stringdist_join.html)
returns the best fuzzy matches using multiple distance methods
(e.g. “lv”, “jw”, “lcs”). See `??stringdist` for more information about
these distance metrics.

The results are filtered for these conditions:

- The variable in `crop_type` must match (e.g. “wheat” would not match
  “barley”)

- Numbers located at the end of a variety name or alias must be an exact
  match if there is more than one number present. If there is only
  number at the end of a variety name, the trailing digit does not need
  to match the rest of the name. For example “myvar1” would still match
  with “myvar”, but “myvar123” would not match “myvar”.

The results of `find_fuzzymatch(`) are written to the file
*fuzzymatch.csv* in the directory provided by the user in the
`knitroutputfolder` argument.

``` r
output_fuzzymatch <- 
  find_fuzzymatch(results_rename$nomatch,
                intid_col = "intid",
                select_before = "2021-05-26",
                knitroutputfolder = knitroutputfolder,
                db_folder = codebook_folder)
#> Writing out fuzzymatch.csv

knitr::kable(output_fuzzymatch)
```

| is_truematch | intid | intid_db | variety | variety_db | var_id | db_id | type | type_db | crop_db | crop_type | crop_type_db | date_added_db | method | nursery | wrong_name |
|:---|:---|:---|:---|:---|:---|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|
|  | aliasname101 | lcsaymeric | new name 101 (alias name101) | LCS Aymeric | 10 | 795 | alias | variety | Hard Red Winter Wheat | wheat | wheat | 2021-04-30 | lv dist | SWW | NA |
|  | newname101 | beamer | new name 101 (alias name101) | Beamer | 10 | 462 | variety | variety | Soft White Winter Wheat | wheat | wheat | 2020-06-08 | jw dist;lv dist | SWW | NA |
|  | newname101 | xa1101 | new name 101 (alias name101) | XA1101 | 10 | 757 | variety | variety | Soft White Winter Wheat | wheat | wheat | 2020-06-08 | jw dist;lv dist | SWW | NA |
|  | newname201 | beamer | new name 201 | Beamer | 11 | 462 | variety | variety | Soft White Winter Wheat | wheat | wheat | 2020-06-08 | jw dist | SWW | NA |
|  | newname201 | syclearstone2cl | new name 201 | SY Clearstone 2CL | 11 | 660 | variety | variety | Hard Red Winter Wheat | wheat | wheat | 2020-06-08 | jw dist_scaled | SWW | NA |
|  | new101 | lww101018 | new101 | LWW10-1018 | 12 | 584 | variety | variety | Soft White Winter Wheat | NA | wheat | 2020-06-08 | jw dist;lcs dist_scaled | NA | NA |
|  | new101 | lww101073 | new101 | LWW10-1073 | 12 | 585 | variety | variety | Soft White Winter Wheat | NA | wheat | 2020-06-08 | jw dist;lcs dist_scaled | NA | NA |
|  | new101 | xa1101 | new101 | XA1101 | 12 | 757 | variety | variety | Soft White Winter Wheat | NA | wheat | 2020-06-08 | lv dist;lv dist_scaled | NA | NA |
|  | wildfir | aacwildfire | wildfir | AAC Wildfire | 14 | 437 | variety | variety | Hard Red Winter Wheat | wheat | wheat | 2020-06-08 | jw dist;jw dist_scaled;lv dist;lv dist_scaled;lcs dist_scaled | HRW | NA |
|  | new101 | lww101018 | New 101 | LWW10-1018 | 5 | 584 | variety | variety | Soft White Winter Wheat | wheat | wheat | 2020-06-08 | jw dist;lcs dist_scaled | S_W | NA |
|  | new101 | lww101073 | New 101 | LWW10-1073 | 5 | 585 | variety | variety | Soft White Winter Wheat | wheat | wheat | 2020-06-08 | jw dist;lcs dist_scaled | S_W | NA |
|  | new101 | xa1101 | New 101 | XA1101 | 5 | 757 | variety | variety | Soft White Winter Wheat | wheat | wheat | 2020-06-08 | lv dist;lv dist_scaled | S_W | NA |
|  | wa008123 | wa8123 | WA008123 | WA8123 | 7 | 495 | variety | alias | Hard White Spring Wheat | wheat | wheat | 2020-06-08 | jw dist;jw dist_scaled;lv dist;lv dist_scaled;lcs dist_scaled | NA | NA |
| FALSE | name201 | NA | Name 201 | NA | 4 | NA | variety | NA | NA | wheat | NA | NA | NA | SWW | NA |

### Fill out the fuzzymatch.csv

Move *fuzzymatch.csv* to the helper_files/ directory and fill out the
`is_truematch` column. (You can append the file with the ending “\_aux”
to clearly differentiate it between the .csv that will be written to the
output/ directory for each run of the .Rmd.) Fill the `is_truematch`
column with “TRUE” if fuzzymatch is a true match, otherwise fill with
“FALSE”.

Sometimes the match may be clear, however, many matches will need to be
manually checked. Some steps for that:

1.  First, start off with a Google search to determine if there are
    clear usages of both names (name in the raw data and name in the
    database). This would suggest that the names refer to different
    varieties and they are not true matches.

2.  If a Google search doesn’t help clarify whether the name is a true
    match (quite likely, unfortunately), check the possible matches with
    our collaborators.

3.  If there is no clear answer (or any answer), the curator should
    leave `is_truematch` blank as the subsequent steps will remind the
    curator to follow up on these possible matches.

### Process *fuzzymatch_aux.csv* manual matches

1.  Read in the *fuzzymatch_aux.csv* that has been manually filled in
2.  Process the file using
    [`process_fuzzymatch()`](https://idahoagstats.github.io/factcuratoR/reference/process_fuzzymatch.md),
    which will write out 3 files to the working directory:

- *fuzzy_check.csv*, which contains variety names that still need to be
  manually checked for whether the fuzzy match is a true match. Update
  is_truematch in the *helper_files/fuzzymatch_aux.csv* file.

There may be newly detected fuzzy matches for successive runs of the
.Rmd.  
This may occur if there are new names added to the codebook or if the
fuzzymatching function is updated.

Newly detected fuzzy matches need to be added to the
*helper_files/fuzzymatch_aux.csv* file. The new matches have only
“new_fuzzymatch” in the source column. (“new_fuzzymatch;csv” in the
source column means that this was a match for this run of the .Rmd
(“new_fuzzymatch”) and that the name is also already present in the
original fuzzymatch.csv (“csv”).)

- *add_fuzzy_to_cv_rename.csv*, which contains variety names for which
  `is_truematch` is “TRUE.” Manually add the names to *cv_rename.csv*.

- *standardize_new_names.csv*, which contains variety names that have no
  match and need to be standardized before being added to the database.
  See next section for full details.

``` r
fuzzy_status <- 
  read_csv(here::here(auxiliary_files, "fuzzymatch_status.csv")) %>%
  mutate(var_id = as.character(var_id))
#> Rows: 19 Columns: 16
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr  (11): intid, intid_db, variety, variety_db, type, type_db, crop_db, cro...
#> dbl   (2): var_id, db_id
#> lgl   (2): is_truematch, wrong_name
#> date  (1): date_added_db
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

results_fuzzymatch <- 
  process_fuzzymatch(output_fuzzymatch_df = output_fuzzymatch, 
                     aux_fuzzy_status = fuzzy_status,
                     knitroutputfolder = knitroutputfolder)
#> Warning in process_fuzzymatch(output_fuzzymatch_df = output_fuzzymatch, :
#> var_id has changed between the two inputs. Setting aux_fuzzy_status var_id to
#> NA
#> Varieties left to check: 0
#> Writing out fuzzy_check.csv.  Check these fuzzymatches.
#>             Add source = new_fuzzymatches to the aux_fuzzy_status file.
#> Writing add_fuzzy_to_cv_rename.csv  Add these names to the main cv_rename.csv
#> Writing standardize_new_names.csv.
#>           If a cultivar has more than one spelling or formatting,
#>           standardize the name in the new_std_name column
#> match: 2 nomatch: 5 check: 0 not_needed: 0
```

### Standardize names without a match

1.  Move *standardize_new_names.csv* to the helper_files/ directory (You
    can append the file with the ending “\_aux” to clearly differentiate
    it between the .csv that will be written to the output/ directory
    for each run of the .Rmd.)

2.  Go through and standardize the names in
    *standardize_new_names_aux.csv* by editing the names in the column
    `new_std_name`. The function
    [`process_fuzzymatch()`](https://idahoagstats.github.io/factcuratoR/reference/process_fuzzymatch.md)
    will make an initial guess by populating `new_std_name` with the the
    raw variety name.

Note on duplicate names: It is especially important if there are
different spellings of the same variety, the names are standardized in
this file. For example, if “WA0061” and “WA61” are the same variety
(spoiler: they are), the `new_std_name` should be the same for both
entries.  
If the names are not standardized, processing the names later will
create two different entries to add to the database controlled
vocabularies.

Note on names missed by fuzzy matching: If any of the names in
*standardize_new_names_aux.csv* match a variety in the database, then
manually add the names to the *controlled_vocab/cv_rename.csv* file.
This occurs when the name is not found by fuzzy matching. The fuzzy
matching functions have been written to return plausible matches while
trying to minimize the number of total returns so to not overwhelm the
user (you). This trade off means that some potential matches may be
missed.

1.  Read in and process the file using
    [`process_std_new_names()`](https://idahoagstats.github.io/factcuratoR/reference/process_std_new_names.md),
    which will write out files to the /output folder:

- *add_to_new_std_name.csv.*, which contains names that have newly
  passed from the fuzzy matching stage to the name processing stage. If
  there are entries in this file, add the names to
  *helper_files/standardize_new_names_aux.csv* and standardize the
  `std_new_name` column as described above.
- *add_to_cv_rename.csv*, which are names that have been updated in
  *standardize_new_names_aux.csv* and won’t match by exact_matching
  (i.e. if the `std_new_name` was changed from the default value. If
  there are entries in this file, add the names to
  *controlled_vocab/cv_rename.csv*
- *add_to_db.csv*, which contains new varieties. Add these entries to
  the database. May need to manually fill in extra information such as
  crop or alias

``` r
results_new_names <- 
  process_std_new_names(
    output_nomatch_df = results_fuzzymatch[["nomatch"]], 
    auxiliary_files = auxiliary_files,  
    filename = "standardize_new_names_aux.csv",
    knitroutputfolder = knitroutputfolder,
    db_folder = codebook_folder)
#> Joining with `by = join_by(var_id, variety, intid, type, crop_type)`
#> Writing add_to_new_std_name.csv. Add these entries to
#> auxiliary_files/standardize_new_names*.csv
#> Joining with `by = join_by(comment)`
#> Status: # A tibble: 3 × 4 comment n required req <chr> <int> <lgl> <int> 1
#> exists in both data and codebook 1 NA NA 2 not present in codebook: cultivar 10
#> NA NA 3 not present in data 10 TRUE 2
#> Adding columns: crop, date_added, alias, pedigree, year_released, usage,
#> usda_pi_number, pvp, patent, comments
```

### Iterate through the process

If the fuzzy_status file is updated (that is, `is_truematch` is filled
in), re-run the entire process in a clean R session and
[`process_fuzzymatch()`](https://idahoagstats.github.io/factcuratoR/reference/process_fuzzymatch.md)
and
[`process_std_new_names()`](https://idahoagstats.github.io/factcuratoR/reference/process_std_new_names.md)
will update the output files.

The goal is to iterate through the process outlined above until all of
the output files (*fuzzy_check.csv*, *add_to_cv_rename.csv*,
*add_to_new_std_name.csv*, and *add_to_new_std_name.csv*) are empty.

When this has been completed, all of the variety names should be matched
and all the necessary information has been moved into the controlled
vocabularies.

Hooray! Make sure you celebrate this achievement!

## Collect all the matches from each step

Once the names from the
[`process_std_new_names()`](https://idahoagstats.github.io/factcuratoR/reference/process_std_new_names.md)
step have been added to the *cv_rename* file, then they should match in
the cv_rename step.

The
function[`collect_final_matches()`](https://idahoagstats.github.io/factcuratoR/reference/collect_final_matches.md)
is used to collect all the matches and the stage where they were
matched.

``` r
names_matches <- 
  collect_final_matches(list(exact = results_exactmatch,
                             rename = results_rename,
                             fuzzy = results_fuzzymatch), 
                        names_raw,
                        knitroutputfolder = knitroutputfolder)
#> Warning: Unknown columns: `intid_db_db`, `wrong_name`, `method`, `source`
#> Warning: Unknown columns: `crop_db`, `method`, `source`
#> Warning: Unknown columns: `intid_db_db`
#> Warning:  5 cultivar(s) not yet matched to the database.
#>                       The raw name is being returned
#> Writing final_matches.csv

knitr::kable(names_matches)
```

| variety | crop_type | nursery | type | intid | var_id | n_var_id | variety_db | match_step | intid_matches | intid_db | type_db |
|:---|:---|:---|:---|:---|:---|---:|:---|:---|:---|:---|:---|
| 455 | wheat | NA | variety | 455 | 1 | 1 | 455 | exact | 455 | 455 | variety |
| AAC wildfire | wheat | H_W | variety | aacwildfire | 2 | 1 | AAC Wildfire | exact | aacwildfire | aacwildfire | variety |
| AAC wildfire (W512) | wheat | HRW | variety | aacwildfire | 3 | 2 | AAC Wildfire | exact | aacwildfire | aacwildfire | variety |
| Name 201 | wheat | SWW | variety | name201 | 4 | 1 | Name 201 | NA | NA | NA | NA |
| New 101 | wheat | S_W | variety | new101 | 5 | 1 | New 101 | NA | NA | NA | NA |
| W512 | wheat | H_W;HRW | variety | w512 | 6 | 1 | W512 | exact | w512 | w512 | alias |
| WA008123 | wheat | NA | variety | wa008123 | 7 | 1 | WA8123 | fuzzy | wa008123 | wa8123 | alias |
| Wildfile (W512) | wheat | HRW | variety | wildfile | 8 | 2 | W512 | exact | w512 | w512 | alias |
| melbclub | wheat | NA | variety | melbclub | 9 | 1 | Melba | rename | melbclub | melbclub | NA |
| new name 101 (alias name101) | wheat | SWW | variety | newname101 | 10 | 2 | new name 101 (alias name101) | NA | NA | NA | NA |
| new name 201 | wheat | SWW | variety | newname201 | 11 | 1 | new name 201 | NA | NA | NA | NA |
| new101 | NA | NA | variety | new101 | 12 | 1 | new101 | NA | NA | NA | NA |
| wb1035cl | wheat | NA | variety | wb1035cl | 13 | 1 | WB-1035CL+ | rename | wb1035cl | wb1035cl | NA |
| wildfir | wheat | HRW | variety | wildfir | 14 | 1 | AAC Wildfire | fuzzy | wildfir | aacwildfire | variety |

## Bind matches with the data

``` r
# do not run this code (it's here for example only)
data_cv_names <- left_join(data, names_matches, by = "variety")
```

## Special instructions for matching blends

Curate the blends separately from non-blend variety names. Below is an
example for curating blends.

``` r
blends <- read_csv(here::here("tests/testthat/test_match_variety_files", 
                              "example_blends.csv"), 
                col_types = cols())

knitr::kable(blends)
```

| variety            | crop_type | behavior_to_Test           |
|:-------------------|:----------|:---------------------------|
| wildfire/ altigo   | wheat     | wildfire won’t exact match |
| APcoachman/ alpowa | wheat     | both should exact match    |
| coachman/alpwa     | wheat     | both won’t exact match     |

### Create unique internal id for names

Note! For blends, set the argument `is_blends = TRUE` for each of the
variety matching functions.

``` r
blends_raw <- 
  create_intid(blends, 
               variety, 
               # Although sep_alias is used to separate 
               # an alias from the variety name, it can also be used here 
               #to separate two names in a blend
               sep_aliases = "\\/", 
               crop_type,
               is_blends = TRUE) 

knitr::kable(blends_raw)
```

| variety            | crop_type | type   | intid      | var_id |
|:-------------------|:----------|:-------|:-----------|:-------|
| APcoachman/ alpowa | wheat     | blends | apcoachman | 1      |
| APcoachman/ alpowa | wheat     | blends | alpowa     | 1      |
| coachman/alpwa     | wheat     | blends | coachman   | 2      |
| coachman/alpwa     | wheat     | blends | alpwa      | 2      |
| wildfire/ altigo   | wheat     | blends | wildfire   | 3      |
| wildfire/ altigo   | wheat     | blends | altigo     | 3      |

### Do exact matching step

``` r
blends_exactmatch <- 
  do_exactmatch(db_folder = codebook_folder,
                data_intid = blends_raw,
                select_before = "2021-11-22",
                match_type = "raw",
                is_blends = TRUE)
#> match: 3 nomatch: 3 check: 0 not_needed: 0

knitr::kable(blends_exactmatch[["match"]])
```

| variety | crop_type | type | intid | var_id | db_id | crop_db | crop_type_db | date_added_db | type_db | variety_db | intid_db | is_truematch | any_match |
|:---|:---|:---|:---|:---|---:|:---|:---|:---|:---|:---|:---|:---|:---|
| APcoachman/ alpowa | wheat | blends | apcoachman | 1 | 451 | Soft White Spring Wheat | wheat | 2020-06-08 | variety | AP Coachman | apcoachman | TRUE | TRUE |
| APcoachman/ alpowa | wheat | blends | alpowa | 1 | 442 | Soft White Spring Wheat | wheat | 2020-06-08 | variety | Alpowa | alpowa | TRUE | TRUE |
| wildfire/ altigo | wheat | blends | altigo | 3 | 443 | Hard Red Winter Wheat | wheat | 2020-06-08 | variety | Altigo | altigo | TRUE | TRUE |

``` r
knitr::kable(blends_exactmatch[["nomatch"]])
```

| variety          | crop_type | type   | intid    | var_id |
|:-----------------|:----------|:-------|:---------|:-------|
| coachman/alpwa   | wheat     | blends | coachman | 2      |
| coachman/alpwa   | wheat     | blends | alpwa    | 2      |
| wildfire/ altigo | wheat     | blends | wildfire | 3      |

``` r
blends_rename <- 
  do_exactmatch(db_folder = codebook_folder, 
                data_intid = blends_exactmatch$nomatch, 
                match_type = "raw", 
                rename_df = TRUE, 
                is_blends = TRUE,
                rename_df_path = here::here(codebook_folder, "cv_rename.csv"))
#> Rows: 147 Columns: 4
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (4): correct_variety_name, wrong_name, program, crop_type
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> match: 2 nomatch: 1 check: 0 not_needed: 0

knitr::kable(blends_rename[["match"]])
```

| variety | crop_type | type | intid | var_id | intid_db_db | variety_db | wrong_name | crop_type_db | type_db | intid_db | is_truematch | any_match |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| coachman/alpwa | wheat | blends | alpwa | 2 | alpowa | Alpowa | alpwa | wheat | NA | alpwa | TRUE | TRUE |
| wildfire/ altigo | wheat | blends | wildfire | 3 | aacwildfire | AAC Wildfire | Wildfire | NA | NA | wildfire | TRUE | TRUE |

``` r
knitr::kable(blends_rename[["nomatch"]])
```

| variety        | crop_type | type   | intid    | var_id | wrong_name |
|:---------------|:----------|:-------|:---------|:-------|:-----------|
| coachman/alpwa | wheat     | blends | coachman | 2      | NA         |

### Do fuzzymatching step

``` r
blends_fuzzymatch <- 
  find_fuzzymatch(blends_rename$nomatch,
                intid_col = "intid",
                select_before = "2021-05-26",
                knitroutputfolder = knitroutputfolder_blends,
                db_folder = codebook_folder)
#> Writing out fuzzymatch.csv

knitr::kable(blends_fuzzymatch)
```

| is_truematch | intid | intid_db | variety | variety_db | var_id | db_id | type | type_db | crop_db | crop_type | crop_type_db | date_added_db | method | wrong_name |
|:---|:---|:---|:---|:---|:---|---:|:---|:---|:---|:---|:---|:---|:---|:---|
|  | coachman | apcoachman | coachman/alpwa | AP Coachman | 2 | 451 | blends | variety | Soft White Spring Wheat | wheat | wheat | 2020-06-08 | jw dist;jw dist_scaled;lv dist;lv dist_scaled;lcs dist_scaled | NA |

### Read in and process fuzzymatches

``` r
blends_fuzzy_status <- 
  read_csv(here::here(auxiliary_files, "fuzzymatch_blends_status.csv")) %>%
  mutate(var_id = as.character(var_id))
#> Rows: 1 Columns: 15
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr  (10): intid, intid_db, variety, variety_db, type, type_db, crop_db, cro...
#> dbl   (2): var_id, db_id
#> lgl   (2): is_truematch, wrong_name
#> date  (1): date_added_db
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

blends_results_fuzzymatch <- 
  process_fuzzymatch(output_fuzzymatch_df = blends_fuzzymatch, 
                     aux_fuzzy_status = blends_fuzzy_status,
                     knitroutputfolder = knitroutputfolder_blends)
#> Varieties left to check: 1
#> Writing out fuzzy_check.csv.  Check these fuzzymatches.
#>             Add source = new_fuzzymatches to the aux_fuzzy_status file.
#> Writing add_fuzzy_to_cv_rename.csv  Add these names to the main cv_rename.csv
#> Writing standardize_new_names.csv.
#>           If a cultivar has more than one spelling or formatting,
#>           standardize the name in the new_std_name column
#> match: 0 nomatch: 0 check: 1 not_needed: 0
```

### Collect all matches

``` r
blends_matches <- 
  collect_final_matches(list(exact = blends_exactmatch,
                             rename = blends_rename,
                             fuzzy = blends_results_fuzzymatch), 
                        blends_raw,
                        knitroutputfolder = knitroutputfolder_blends,
                        is_blends = TRUE)
#> Warning: Unknown columns: `intid_db_db`, `wrong_name`, `method`, `source`
#> Warning: Unknown columns: `crop_db`, `method`, `source`
#> Warning: Unknown columns: `intid_db_db`
#> Warning:  1 cultivar(s) not yet matched to the database.
#>                       The raw name is being returned
#> Writing final_matches.csv

knitr::kable(blends_matches)
```

| var_id | variety | crop_type | type | intid | n_var_id | variety_db | match_step | intid_db | type_db |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| 1 | APcoachman/ alpowa | wheat | blends | apcoachman;alpowa | 2 | AP Coachman;Alpowa | exact | apcoachman;alpowa | variety |
| 2 | coachman/alpwa | wheat | blends | coachman;alpwa | 2 | coachman;Alpowa | rename | alpwa |  |
| 3 | wildfire/ altigo | wheat | blends | wildfire;altigo | 2 | AAC Wildfire;Altigo | rename;exact | wildfire;altigo | variety |
