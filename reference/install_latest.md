# Update latest ggeffects-version from R-universe (GitHub) or CRAN

This function can be used to install the latest package version of
*ggeffects*, either the development version (from R-universe/GitHub) or
the current version from CRAN.

## Usage

``` r
install_latest(
  source = c("development", "cran"),
  force = FALSE,
  verbose = TRUE
)
```

## Arguments

- source:

  Character. Either `"development"` or `"cran"`. If `"cran"`,
  *ggeffects* will be installed from the default CRAN mirror returned by
  `getOption("repos")['CRAN']`. If `"development"` (the default),
  *ggeffects* is installed from the r-universe repository
  (<https://strengejacke.r-universe.dev/>).

- force:

  Logical, if `FALSE`, the update will only be installed if a newer
  version is available. Use `force=TRUE` to force installation, even if
  the version number for the locally installed package is identical to
  the latest development-version. Only applies when
  `source="development"`.

- verbose:

  Toggle messages.

## Value

Invisible `NULL`.

## Examples

``` r
if (FALSE) {
# install latest development-version of ggeffects from the
# r-universe repository
install_latest()
}
```
