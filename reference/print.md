# Print and format ggeffects-objects

A generic print-method for `ggeffects`-objects.

## Usage

``` r
# S3 method for class 'ggeffects'
format(
  x,
  variable_labels = FALSE,
  value_labels = FALSE,
  group_name = FALSE,
  row_header_separator = ", ",
  digits = 2,
  collapse_ci = FALSE,
  collapse_tables = FALSE,
  n,
  ...
)

# S3 method for class 'ggcomparisons'
format(x, collapse_ci = FALSE, collapse_p = FALSE, combine_levels = FALSE, ...)

# S3 method for class 'ggeffects'
print(x, group_name = TRUE, digits = 2, verbose = TRUE, ...)

# S3 method for class 'ggeffects'
print_md(x, group_name = TRUE, digits = 2, ...)

# S3 method for class 'ggeffects'
print_html(
  x,
  group_name = TRUE,
  digits = 2,
  theme = NULL,
  engine = c("tt", "gt"),
  ...
)

# S3 method for class 'ggcomparisons'
print(x, collapse_tables = TRUE, ...)

# S3 method for class 'ggcomparisons'
print_html(
  x,
  collapse_ci = FALSE,
  collapse_p = FALSE,
  theme = NULL,
  engine = c("tt", "gt"),
  ...
)

# S3 method for class 'ggcomparisons'
print_md(x, collapse_ci = FALSE, collapse_p = FALSE, theme = NULL, ...)
```

## Arguments

- x:

  An object of class `ggeffects`, as returned by the functions from this
  package.

- variable_labels:

  Logical, if `TRUE` variable labels are used as column headers. If
  `FALSE`, variable names are used.

- value_labels:

  Logical, if `TRUE`, value labels are used as values in the table
  output. If `FALSE`, the numeric values or factor levels are used.

- group_name:

  Logical, if `TRUE`, the name of further focal terms are used in the
  sub-headings of the table. If `FALSE`, only the values of the focal
  terms are used.

- row_header_separator:

  Character, separator between the different subgroups in the table
  output.

- digits:

  Number of digits to print.

- collapse_ci:

  Logical, if `TRUE`, the columns with predicted values and confidence
  intervals are collapsed into one column, e.g. `Predicted (95% CI)`.

- collapse_tables:

  Logical, if `TRUE`, all tables are combined into one. The tables are
  not split by further focal terms, but rather are added as columns.
  Only works when there is more than one focal term.

- n:

  Number of rows to print per subgroup. If `NULL`, a default number of
  rows is printed, depending on the number of subgroups.

- ...:

  Further arguments passed down to `format.ggeffects()`, some of them
  are also passed down further to
  [`insight::format_table()`](https://easystats.github.io/insight/reference/format_table.html)
  or
  [`insight::format_value()`](https://easystats.github.io/insight/reference/format_value.html).

- collapse_p:

  Logical, if `TRUE`, the columns with predicted values and p-values are
  collapsed into one column, where significant p-values are indicated as
  asterisks.

- combine_levels:

  Logical, if `TRUE`, the levels of the first comparison of each focal
  term against the second are combined into one column. This is useful
  when comparing multiple focal terms, e.g. `education = low-high` and
  `gender = male-female` are combined into `first = low-male` and
  `second = high-female`.

- verbose:

  Toggle messages.

- theme:

  The theme to apply to the table. One of `"grid"`, `"striped"`,
  `"bootstrap"`, or `"darklines"`.

- engine:

  The engine to use for printing. One of `"tt"` (default) or `"gt"`.
  `"tt"` uses the *tinytable* package, `"gt"` uses the *gt* package.

## Value

[`format()`](https://rdrr.io/r/base/format.html) return a formatted data
frame, `print()` prints a formatted data frame to the console.
[`print_html()`](https://easystats.github.io/insight/reference/display.html)
returns a `tinytable` object by default (unless changed with
`engine = "gt"`), which is printed as HTML, markdown or LaTeX table
(depending on the context from which
[`print_html()`](https://easystats.github.io/insight/reference/display.html)
is called, see
[`tinytable::tt()`](https://vincentarelbundock.github.io/tinytable/man/tt.html)
for details).

## Global Options to Customize Tables when Printing

The `verbose` argument can be used to display or silence messages and
warnings. Furthermore,
[`options()`](https://rdrr.io/r/base/options.html) can be used to set
defaults for the `print()` and
[`print_html()`](https://easystats.github.io/insight/reference/display.html)
method. The following options are available, which can simply be run in
the console:

- `ggeffects_ci_brackets`: Define a character vector of length two,
  indicating the opening and closing parentheses that encompass the
  confidence intervals values, e.g.
  `options(ggeffects_ci_brackets = c("[", "]"))`.

- `ggeffects_collapse_ci`: Logical, if `TRUE`, the columns with
  predicted values (or contrasts) and confidence intervals are collapsed
  into one column, e.g. `options(ggeffects_collapse_ci = TRUE)`.

- `ggeffects_collapse_p`: Logical, if `TRUE`, the columns with predicted
  values (or contrasts) and p-values are collapsed into one column, e.g.
  `options(ggeffects_collapse_p = TRUE)`. Note that p-values are
  replaced by asterisk-symbols (stars) or empty strings when
  `ggeffects_collapse_p = TRUE`, depending on the significance level.

- `ggeffects_collapse_tables`: Logical, if `TRUE`, multiple tables for
  subgroups are combined into one table. Only works when there is more
  than one focal term, e.g. `options(ggeffects_collapse_tables = TRUE)`.

- `ggeffects_output_format`: String, either `"text"`, `"markdown"` or
  `"html"`. Defines the default output format from
  [`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md).
  If `"html"`, a formatted HTML table is created and printed to the view
  pane. `"markdown"` creates a markdown-formatted table inside Rmarkdown
  documents, and prints a text-format table to the console when used
  interactively. If `"text"` or `NULL`, a formatted table is printed to
  the console, e.g. `options(ggeffects_output_format = "html")`.

- `ggeffects_html_engine`: String, either `"tt"` or `"gt"`. Defines the
  default engine to use for printing HTML tables. If `"tt"`, the
  *tinytable* package is used, if `"gt"`, the *gt* package is used, e.g.
  `options(ggeffects_html_engine = "gt")`.

Use `options(<option_name> = NULL)` to remove the option.

## Examples

``` r
data(efc, package = "ggeffects")
fit <- lm(barthtot ~ c12hour + e42dep, data = efc)

# default print
predict_response(fit, "e42dep")
#> # Predicted values of Total score BARTHEL INDEX
#> 
#> e42dep | Predicted |         95% CI
#> -----------------------------------
#>      1 |    104.42 | 101.21, 107.64
#>      2 |     83.81 |  81.91,  85.72
#>      3 |     63.20 |  61.94,  64.47
#>      4 |     42.59 |  40.54,  44.65
#> 
#> Adjusted for:
#> * c12hour = 42.25

# surround CI values with parentheses
print(predict_response(fit, "e42dep"), ci_brackets = c("(", ")"))
#> # Predicted values of Total score BARTHEL INDEX
#> 
#> e42dep | Predicted |           95% CI
#> -------------------------------------
#>      1 |    104.42 | (101.21, 107.64)
#>      2 |     83.81 | ( 81.91,  85.72)
#>      3 |     63.20 | ( 61.94,  64.47)
#>      4 |     42.59 | ( 40.54,  44.65)
#> 
#> Adjusted for:
#> * c12hour = 42.25
# you can also use `options(ggeffects_ci_brackets = c("[", "]"))`
# to set this globally

# collapse CI columns into column with predicted values
print(predict_response(fit, "e42dep"), collapse_ci = TRUE)
#> # Predicted values of Total score BARTHEL INDEX
#> 
#> e42dep |      Predicted (95% CI)
#> --------------------------------
#>      1 | 104.42 (101.21, 107.64)
#>      2 |  83.81  (81.91,  85.72)
#>      3 |  63.20  (61.94,  64.47)
#>      4 |  42.59  (40.54,  44.65)
#> 
#> Adjusted for:
#> * c12hour = 42.25

# include value labels
print(predict_response(fit, "e42dep"), value_labels = TRUE)
#> # Predicted values of Total score BARTHEL INDEX
#> 
#>                   e42dep | Predicted |         95% CI
#> -----------------------------------------------------
#>          [1] independent |    104.42 | 101.21, 107.64
#>   [2] slightly dependent |     83.81 |  81.91,  85.72
#> [3] moderately dependent |     63.20 |  61.94,  64.47
#>   [4] severely dependent |     42.59 |  40.54,  44.65
#> 
#> Adjusted for:
#> * c12hour = 42.25

# include variable labels in column headers
print(predict_response(fit, "e42dep"), variable_labels = TRUE)
#> # Predicted values of Total score BARTHEL INDEX
#> 
#> elder's dependency | Predicted values of Total score BARTHEL INDEX |         95% CI
#> -----------------------------------------------------------------------------------
#>                  1 |                                        104.42 | 101.21, 107.64
#>                  2 |                                         83.81 |  81.91,  85.72
#>                  3 |                                         63.20 |  61.94,  64.47
#>                  4 |                                         42.59 |  40.54,  44.65
#> 
#> Adjusted for:
#> * c12hour = 42.25

# include value labels and variable labels
print(predict_response(fit, "e42dep"), variable_labels = TRUE, value_labels = TRUE)
#> # Predicted values of Total score BARTHEL INDEX
#> 
#>       elder's dependency | Predicted values of Total score BARTHEL INDEX |         95% CI
#> -----------------------------------------------------------------------------------------
#>          [1] independent |                                        104.42 | 101.21, 107.64
#>   [2] slightly dependent |                                         83.81 |  81.91,  85.72
#> [3] moderately dependent |                                         63.20 |  61.94,  64.47
#>   [4] severely dependent |                                         42.59 |  40.54,  44.65
#> 
#> Adjusted for:
#> * c12hour = 42.25

data(iris)
m <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)

# default print with subgroups
predict_response(m, c("Petal.Length", "Species"))
#> # Predicted values of Sepal.Length
#> 
#> Species: setosa
#> 
#> Petal.Length | Predicted |      95% CI
#> --------------------------------------
#>         1.00 |      4.76 | 4.49,  5.03
#>         2.00 |      5.30 | 4.99,  5.61
#>         3.00 |      5.84 | 4.99,  6.69
#>         4.00 |      6.38 | 4.99,  7.77
#>         5.00 |      6.92 | 4.99,  8.86
#>         7.00 |      8.01 | 4.98, 11.04
#> 
#> Species: versicolor
#> 
#> Petal.Length | Predicted |      95% CI
#> --------------------------------------
#>         1.00 |      3.24 | 2.57,  3.90
#>         2.00 |      4.06 | 3.60,  4.53
#>         3.00 |      4.89 | 4.62,  5.16
#>         4.00 |      5.72 | 5.61,  5.83
#>         5.00 |      6.55 | 6.37,  6.73
#>         7.00 |      8.21 | 7.64,  8.77
#> 
#> Species: virginica
#> 
#> Petal.Length | Predicted |      95% CI
#> --------------------------------------
#>         1.00 |      2.06 | 1.27,  2.84
#>         2.00 |      3.05 | 2.43,  3.67
#>         3.00 |      4.05 | 3.60,  4.50
#>         4.00 |      5.04 | 4.76,  5.33
#>         5.00 |      6.04 | 5.90,  6.17
#>         7.00 |      8.03 | 7.76,  8.30
#> 
#> 
#> Not all rows are shown in the output. Use `print(..., n = Inf)` to show
#>   all rows.

# omit name of grouping variable in subgroup table headers
print(predict_response(m, c("Petal.Length", "Species")), group_name = FALSE)
#> # Predicted values of Sepal.Length
#> 
#> setosa
#> 
#> Petal.Length | Predicted |      95% CI
#> --------------------------------------
#>         1.00 |      4.76 | 4.49,  5.03
#>         2.00 |      5.30 | 4.99,  5.61
#>         3.00 |      5.84 | 4.99,  6.69
#>         4.00 |      6.38 | 4.99,  7.77
#>         5.00 |      6.92 | 4.99,  8.86
#>         7.00 |      8.01 | 4.98, 11.04
#> 
#> versicolor
#> 
#> Petal.Length | Predicted |      95% CI
#> --------------------------------------
#>         1.00 |      3.24 | 2.57,  3.90
#>         2.00 |      4.06 | 3.60,  4.53
#>         3.00 |      4.89 | 4.62,  5.16
#>         4.00 |      5.72 | 5.61,  5.83
#>         5.00 |      6.55 | 6.37,  6.73
#>         7.00 |      8.21 | 7.64,  8.77
#> 
#> virginica
#> 
#> Petal.Length | Predicted |      95% CI
#> --------------------------------------
#>         1.00 |      2.06 | 1.27,  2.84
#>         2.00 |      3.05 | 2.43,  3.67
#>         3.00 |      4.05 | 3.60,  4.50
#>         4.00 |      5.04 | 4.76,  5.33
#>         5.00 |      6.04 | 5.90,  6.17
#>         7.00 |      8.03 | 7.76,  8.30
#> 
#> 
#> Not all rows are shown in the output. Use `print(..., n = Inf)` to show
#>   all rows.

# collapse tables into one
print(predict_response(m, c("Petal.Length", "Species")), collapse_tables = TRUE, n = 3)
#> # Predicted values of Sepal.Length
#> 
#> Petal.Length |    Species | Predicted |      95% CI
#> ---------------------------------------------------
#>         1.00 |     setosa |      4.76 | 4.49,  5.03
#>         3.00 |            |      5.84 | 4.99,  6.69
#>         7.00 |            |      8.01 | 4.98, 11.04
#>         1.00 | versicolor |      3.24 | 2.57,  3.90
#>         3.00 |            |      4.89 | 4.62,  5.16
#>         7.00 |            |      8.21 | 7.64,  8.77
#>         1.00 |  virginica |      2.06 | 1.27,  2.84
#>         3.00 |            |      4.05 | 3.60,  4.50
#>         7.00 |            |      8.03 | 7.76,  8.30
#> 
#> 
#> Not all rows are shown in the output. Use `print(..., n = Inf)` to show
#>   all rows.

# increase number of digits
print(predict_response(fit, "e42dep"), digits = 5)
#> # Predicted values of Total score BARTHEL INDEX
#> 
#> e42dep | Predicted |               95% CI
#> -----------------------------------------
#>      1 | 104.42339 | 101.21081, 107.63598
#>      2 |  83.81330 |  81.90728,  85.71932
#>      3 |  63.20320 |  61.93656,  64.46985
#>      4 |  42.59311 |  40.53516,  44.65105
#> 
#> Adjusted for:
#> * c12hour = 42.25
```
