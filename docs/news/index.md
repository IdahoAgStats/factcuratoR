# Changelog

## factcuratoR 1.2.1

2026-01-06

- Updating R version and packages to R 4.5.2 caused a breaking change to
  the factcuratoR fuzzymatch module. The underlying method for the
  internal function
  [`is_string_overlap()`](https://idahoagstats.github.io/factcuratoR/reference/is_string_overlap.md)
  was changed from stringr to
  [`stringi::stri_detect_fixed()`](https://rdrr.io/pkg/stringi/man/stri_detect.html)
  to handle NAs in the string-comparison tests. `output_fuzzymatch()` is
  back to working as expected.

- factcuratoR now depends on R \>= 4.1.0 due to integration of the
  native pipe operator `|>` and transition to modern syntax, i.e.,
  `\(x, idx)` and away from ambiguous dot notation (., .x, .y).
