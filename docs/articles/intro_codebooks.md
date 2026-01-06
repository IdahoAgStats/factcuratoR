# Introduction to the codebook structure

## Introduction

The purpose of **factcuratoR** is to provide sets of functions to help
standardize variety testing data for curation of variety testing data
sets supporting the WAVE project.

The goal of the WAVE program curation is to generate *trial data* and
*trial metadata* that conform to the controlled vocabulary codebooks.

## Trial data and metadata

1.  Trial data consists of multiple plot entries per trial. This
    includes all data applicable at the plot-level, such as plot,
    variety, entry, and traits like height, stand, and moisture.

2.  Metadata consists of one entry per trial. Metadata is trial-level
    data, such as location, year, program, and nursery.

## Overview of codebook structure

First, load **factcuratoR** and point to the main codebook (which
currently must be named *codebooks_all_db.csv* for the validation
functions)

``` r
library(factcuratoR)

rlang::check_installed("here")

codebook_folder <- here::here(
  "tests/testthat/test_controlled_vocab")
```

The main codebook contains the variable names and required status for
the trial data and the trial metadata. For example, the trial data
should contain the columns trial, variety, entry, and plot. The trial
metadata should contain columns for trial, nursery, year, location, etc.

Note! Traits are collected for each trial, so conceptually, it would
make sense for the traits (such as `test_weight` or `height`) to be
contained in the main codebook associated with trial_data. However,
because the traits list is expected to get rather long, the traits are
stored in a separate file called *traits.csv*. For validating the
columns in the trial data, the validation functions pull in the traits
and treats each trait as a column in the trial data.

``` r
create_dm(here::here(codebook_folder, "codebooks_all_db.csv"))
```

Fig. 1 Example codebook structure

The file *codebooks_all_db.csv* also specifies whether a variable must
conform to controlled vocabularies. If so, the controlled vocabularies
are listed in another codebook (e.g. allowed levels of nursery in
`trials_metadata` are defined in the nursery codebook). Let’s call these
‘controlled vocabulary codebooks’ to better distinguish them from the
‘main codebook.’

Said another way, the ‘main codebook’ hold column names and the
‘controlled vocabulary codebooks’ hold levels that are approved as
column contents.

Just as the main codebook defines the columns that are present in the
trial_data and trials_metadata,\* codebooks_all_db.csv\* also defines
the columns that are present in the controlled vocabulary codebooks. For
the example (Fig. 1), *codebooks_all_db.csv* also has information for
the column names in the cultivar, nursery, location, and
crop_market_classes controlled vocabulary codebooks.

## More about codebooks_all_db.csv

The codebooks_all_db.csv has the following (required) columns:

- `book`: name of the codebook  
- `variable`: name of a column in the trial data, metadata, or a
  codebook  
- `value_type`: A level matching one of the following options:
  - categorical: type if the variable must conform to controlled
    vocabularies (acceptable variables must be specified in
    values_defined_in or value_range)  
  - string: uncontrolled string (validation function will check to
    ensure `is.character() == TRUE`)  
  - integer (validation will check `%%1 == 0`)  
  - continuous (acceptable range must be provided in `value_range`)  
  - date (validation will check for yyyy-mm-dd format)  
- `meaning`: description of the variable, including units or formatting
  requirements
- `values_defined_in`: This is NA if value_type != “categorical.”
  Otherwise, this field should be populated with a name that matches a
  book where the variable controlled vocabulary is defined (e.g. for
  location in trials_metadata, the values_defined_in is “locations”)
  - `value_range`: If this variable does not have controlled
    vocabularies, enter the accepted values or ranges. This formatting
    is used in the *qc_validate_fns.R* to validate the data.
    - Entries should be separated by semi-colons (e.g. “TRUE; FALSE” or
      “1; 2; 3”) \*Strings should be quoted (e.g. “value1”; “value2”)  
    - Range should be in the form: min to max (e.g. 0 to 100)
    - Separate range and values using: \| (e.g. 0 to 100 \| -9) Careful!
      If there is more than one accepted value, make sure to separate
      those with a semi-colon (e.g. 0 to 100 \| -9;-99)
    - Due to Excel formatting issues, negative numbers should be
      prefaced with ‘ but without closing quotes (e.g. “’-180”)
- `primary_key` \[??\]

``` r
codebooks_all <- readin.db(codebook_folder)
#> This function name is being retained for backwards compatibility.
#>           Please use readin_db()

knitr::kable(codebooks_all$codebooks_all_db.csv %>% filter(book == "trial_data"))
```

| book | required | col_num | variable | value_type | meaning | values_defined_in | value_range | notes | type | previous name | max_characters | primary_key |
|:---|:---|---:|:---|:---|:---|:---|:---|:---|:---|:---|---:|:---|
| trial_data | TRUE | 1 | trial | string | unique identifier for a single field trial based on the program, nursery, location, year, and agronomic_trt | NA | NA | defined by the trials_metadata associated with this trial | NA | NA | NA | NA |
| trial_data | TRUE | 2 | variety | categorical | genetically unique crop germplasm | cultivar | NA | NA | NA | NA | NA | NA |
| trial_data | TRUE | 3 | entry | integer | code for a the variety that is assigned to a single variety for a single year | NA | 0 to 500 | NA | NA | NA | NA | NA |
| trial_data | TRUE | 4 | plot | integer | unique identifier for a single plot within a trial | NA | 1 to 1e6 | NA | NA | NA | NA | NA |
| trial_data | TRUE | 6 | range | integer | what range the plot is found in | NA | 1 to 200 | NA | NA | NA | NA | NA |
| trial_data | TRUE | 5 | rep | integer | identifier for a experimental block within a field trial | NA | 1 to 20 | NA | NA | NA | NA | NA |
| trial_data | TRUE | 7 | row | integer | what row the plot is found in | NA | 1 to 200 | NA | NA | NA | NA | NA |

``` r
knitr::kable(codebooks_all$codebooks_all_db.csv %>% filter(book == "locations"))
```

| book | required | col_num | variable | value_type | meaning | values_defined_in | value_range | notes | type | previous name | max_characters | primary_key |
|:---|:---|---:|:---|:---|:---|:---|:---|:---|:---|:---|---:|:---|
| locations | FALSE | 3 | county | string | county where city is located | NA | NA | currently only counties in Idaho, single entry may include multiple counties when a city straddles multiple counties | NA | NA | NA | NA |
| locations | TRUE | 2 | loc_abbr | categorical | location abbreviation | locations | NA | NA | NA | NA | NA | NA |
| locations | TRUE | 1 | location | categorical | nearest city to a field trial | locations | NA | NA | NA | NA | NA | TRUE |
| locations | TRUE | 4 | state | string | State where city is located | NA | “ID”; “OR”; “WA” | NA | NA | NA | NA | NA |

## Updating the controlled vocabularies

When updating any codebook, the *codebooks_all_db.csv* must be updated
to reflect the change.

- A newly added variable column would correspond to a new row in
  *codebooks_all_db.csv*.  
- A changed variable column name would correspond to change in variable
  name in an existing row. Care must be taken as variables may be used
  in several different codebooks. Renames should be consistent across
  codebooks.
