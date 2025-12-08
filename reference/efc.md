# Sample dataset from the EUROFAMCARE project

An SPSS sample data set, imported with the
[`sjlabelled::read_spss()`](https://strengejacke.github.io/sjlabelled/reference/read_spss.html)
function. Consists of 28 variables from 908 observations. The data set
is part of the EUROFAMCARE project, a cross-national survey on informal
caregiving in Europe.

## Examples

``` r
# Attach EFC-data
data(efc)

# Show structure
str(efc)
#> 'data.frame':    908 obs. of  28 variables:
#>  $ c12hour : num  16 148 70 168 168 16 161 110 28 40 ...
#>   ..- attr(*, "label")= chr "average number of hours of care per week"
#>  $ e15relat: num  2 2 1 1 2 2 1 4 2 2 ...
#>   ..- attr(*, "label")= chr "relationship to elder"
#>   ..- attr(*, "labels")= Named num [1:8] 1 2 3 4 5 6 7 8
#>   .. ..- attr(*, "names")= chr [1:8] "spouse/partner" "child" "sibling" "daughter or son -in-law" ...
#>  $ e16sex  : num  2 2 2 2 2 2 1 2 2 2 ...
#>   ..- attr(*, "label")= chr "elder's gender"
#>   ..- attr(*, "labels")= Named num [1:2] 1 2
#>   .. ..- attr(*, "names")= chr [1:2] "male" "female"
#>  $ e17age  : num  83 88 82 67 84 85 74 87 79 83 ...
#>   ..- attr(*, "label")= chr "elder' age"
#>  $ e42dep  : num  3 3 3 4 4 4 4 4 4 4 ...
#>   ..- attr(*, "label")= chr "elder's dependency"
#>   ..- attr(*, "labels")= Named num [1:4] 1 2 3 4
#>   .. ..- attr(*, "names")= chr [1:4] "independent" "slightly dependent" "moderately dependent" "severely dependent"
#>  $ c82cop1 : num  3 3 2 4 3 2 4 3 3 3 ...
#>   ..- attr(*, "label")= chr "do you feel you cope well as caregiver?"
#>   ..- attr(*, "labels")= Named num [1:4] 1 2 3 4
#>   .. ..- attr(*, "names")= chr [1:4] "never" "sometimes" "often" "always"
#>  $ c83cop2 : num  2 3 2 1 2 2 2 2 2 2 ...
#>   ..- attr(*, "label")= chr "do you find caregiving too demanding?"
#>   ..- attr(*, "labels")= Named num [1:4] 1 2 3 4
#>   .. ..- attr(*, "names")= chr [1:4] "Never" "Sometimes" "Often" "Always"
#>  $ c84cop3 : num  2 3 1 3 1 3 4 2 3 1 ...
#>   ..- attr(*, "label")= chr "does caregiving cause difficulties in your relationship with your friends?"
#>   ..- attr(*, "labels")= Named num [1:4] 1 2 3 4
#>   .. ..- attr(*, "names")= chr [1:4] "Never" "Sometimes" "Often" "Always"
#>  $ c85cop4 : num  2 3 4 1 2 3 1 1 2 2 ...
#>   ..- attr(*, "label")= chr "does caregiving have negative effect on your physical health?"
#>   ..- attr(*, "labels")= Named num [1:4] 1 2 3 4
#>   .. ..- attr(*, "names")= chr [1:4] "Never" "Sometimes" "Often" "Always"
#>  $ c86cop5 : num  1 4 1 1 2 3 1 1 2 1 ...
#>   ..- attr(*, "label")= chr "does caregiving cause difficulties in your relationship with your family?"
#>   ..- attr(*, "labels")= Named num [1:4] 1 2 3 4
#>   .. ..- attr(*, "names")= chr [1:4] "Never" "Sometimes" "Often" "Always"
#>  $ c87cop6 : num  1 1 1 1 2 2 2 1 1 1 ...
#>   ..- attr(*, "label")= chr "does caregiving cause financial difficulties?"
#>   ..- attr(*, "labels")= Named num [1:4] 1 2 3 4
#>   .. ..- attr(*, "names")= chr [1:4] "Never" "Sometimes" "Often" "Always"
#>  $ c88cop7 : num  2 3 1 1 1 2 4 2 3 1 ...
#>   ..- attr(*, "label")= chr "do you feel trapped in your role as caregiver?"
#>   ..- attr(*, "labels")= Named num [1:4] 1 2 3 4
#>   .. ..- attr(*, "names")= chr [1:4] "Never" "Sometimes" "Often" "Always"
#>  $ c89cop8 : num  3 2 4 2 4 1 1 3 1 1 ...
#>   ..- attr(*, "label")= chr "do you feel supported by friends/neighbours?"
#>   ..- attr(*, "labels")= Named num [1:4] 1 2 3 4
#>   .. ..- attr(*, "names")= chr [1:4] "never" "sometimes" "often" "always"
#>  $ c90cop9 : num  3 2 3 4 4 1 4 3 3 3 ...
#>   ..- attr(*, "label")= chr "do you feel caregiving worthwhile?"
#>   ..- attr(*, "labels")= Named num [1:4] 1 2 3 4
#>   .. ..- attr(*, "names")= chr [1:4] "never" "sometimes" "often" "always"
#>  $ c160age : num  56 54 80 69 47 56 61 67 59 49 ...
#>   ..- attr(*, "label")= chr "carer' age"
#>  $ c161sex : num  2 2 1 1 2 1 2 2 2 2 ...
#>   ..- attr(*, "label")= chr "carer's gender"
#>   ..- attr(*, "labels")= Named num [1:2] 1 2
#>   .. ..- attr(*, "names")= chr [1:2] "Male" "Female"
#>  $ c172code: num  2 2 1 2 2 2 2 2 NA 2 ...
#>   ..- attr(*, "label")= chr "carer's level of education"
#>   ..- attr(*, "labels")= Named num [1:3] 1 2 3
#>   .. ..- attr(*, "names")= chr [1:3] "low level of education" "intermediate level of education" "high level of education"
#>  $ c175empl: num  1 1 0 0 0 1 0 0 0 0 ...
#>   ..- attr(*, "label")= chr "are you currently employed?"
#>   ..- attr(*, "labels")= Named num [1:2] 0 1
#>   .. ..- attr(*, "names")= chr [1:2] "no" "yes"
#>  $ barthtot: num  75 75 35 0 25 60 5 35 15 0 ...
#>   ..- attr(*, "label")= chr "Total score BARTHEL INDEX"
#>  $ neg_c_7 : num  12 20 11 10 12 19 15 11 15 10 ...
#>   ..- attr(*, "label")= chr "Negative impact with 7 items"
#>  $ pos_v_4 : num  12 11 13 15 15 9 13 14 13 13 ...
#>   ..- attr(*, "label")= chr "Positive value with 4 items"
#>  $ quol_5  : num  14 10 7 12 19 8 20 20 8 15 ...
#>   ..- attr(*, "label")= chr "Quality of life 5 items"
#>  $ resttotn: num  0 4 0 2 2 1 0 0 0 1 ...
#>   ..- attr(*, "label")= chr "Job restrictions"
#>  $ tot_sc_e: num  4 0 1 0 1 3 0 1 2 1 ...
#>   ..- attr(*, "label")= chr "Services for elderly"
#>  $ n4pstu  : num  0 0 2 3 2 2 3 1 3 3 ...
#>   ..- attr(*, "label")= chr "Care level"
#>   ..- attr(*, "labels")= Named chr [1:5] "0" "1" "2" "3" ...
#>   .. ..- attr(*, "names")= chr [1:5] "No Care Level" "Care Level 1" "Care Level 2" "Care Level 3" ...
#>  $ nur_pst : num  NA NA 2 3 2 2 3 1 3 3 ...
#>   ..- attr(*, "label")= chr "Care level"
#>   ..- attr(*, "labels")= Named chr [1:3] "1" "2" "3"
#>   .. ..- attr(*, "names")= chr [1:3] "Care Level 1" "Care Level 2" "Care Level 3/3+"
#>  $ grp     : Factor w/ 8 levels "spouse/partner",..: 2 2 1 1 2 2 1 4 2 2 ...
#>   ..- attr(*, "label")= chr "relationship to elder"
#>  $ negc7d  : Factor w/ 2 levels "0","1": 2 2 1 1 2 2 2 1 2 1 ...
#>   ..- attr(*, "label")= chr "Negative impact with 7 items"

# show first rows
head(efc)
#>   c12hour e15relat e16sex e17age e42dep c82cop1 c83cop2 c84cop3 c85cop4 c86cop5
#> 1      16        2      2     83      3       3       2       2       2       1
#> 2     148        2      2     88      3       3       3       3       3       4
#> 3      70        1      2     82      3       2       2       1       4       1
#> 4     168        1      2     67      4       4       1       3       1       1
#> 5     168        2      2     84      4       3       2       1       2       2
#> 6      16        2      2     85      4       2       2       3       3       3
#>   c87cop6 c88cop7 c89cop8 c90cop9 c160age c161sex c172code c175empl barthtot
#> 1       1       2       3       3      56       2        2        1       75
#> 2       1       3       2       2      54       2        2        1       75
#> 3       1       1       4       3      80       1        1        0       35
#> 4       1       1       2       4      69       1        2        0        0
#> 5       2       1       4       4      47       2        2        0       25
#> 6       2       2       1       1      56       1        2        1       60
#>   neg_c_7 pos_v_4 quol_5 resttotn tot_sc_e n4pstu nur_pst            grp negc7d
#> 1      12      12     14        0        4      0      NA          child      1
#> 2      20      11     10        4        0      0      NA          child      1
#> 3      11      13      7        0        1      2       2 spouse/partner      0
#> 4      10      15     12        2        0      3       3 spouse/partner      0
#> 5      12      15     19        2        1      2       2          child      1
#> 6      19       9      8        1        3      2       2          child      1
```
