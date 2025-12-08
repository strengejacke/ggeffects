# Create a pretty sequence over a range of a vector

Creates an evenly spaced, pretty sequence of numbers for a range of a
vector.

## Usage

``` r
pretty_range(x, n = NULL, length = NULL)
```

## Arguments

- x:

  A numeric vector.

- n:

  Numeric value, indicating the size of how many values are used to
  create a pretty sequence. If `x` has a large value range (\> 100), `n`
  could be something between 1 to 5. If `x` has a rather small amount of
  unique values, `n` could be something between 10 to 20. If `n = NULL`,
  `pretty_range()` automatically tries to find a pretty sequence.

- length:

  Integer value, as alternative to `n`, defines the number of intervals
  to be returned.

## Value

A numeric vector with a range corresponding to the minimum and maximum
values of `x`. If `x` is missing, a function, pre-programmed with `n`
and `length` is returned. See examples.

## Examples

``` r
data(iris)
# pretty range for vectors with decimal points
pretty_range(iris$Petal.Length)
#>  [1] 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0 5.5 6.0 6.5 7.0

# pretty range for large range, increasing by 50
pretty_range(1:1000)
#>  [1]    0   50  100  150  200  250  300  350  400  450  500  550  600  650  700
#> [16]  750  800  850  900  950 1000

# increasing by 20
pretty_range(1:1000, n = 7)
#>  [1]    0   20   40   60   80  100  120  140  160  180  200  220  240  260  280
#> [16]  300  320  340  360  380  400  420  440  460  480  500  520  540  560  580
#> [31]  600  620  640  660  680  700  720  740  760  780  800  820  840  860  880
#> [46]  900  920  940  960  980 1000

# return 10 intervals
pretty_range(1:1000, length = 10)
#>  [1]    0  100  200  300  400  500  600  700  800  900 1000

# same result
pretty_range(1:1000, n = 2.5)
#>  [1]    0  100  200  300  400  500  600  700  800  900 1000

# function factory
range_n_5 <- pretty_range(n = 5)
range_n_5(1:1000)
#>  [1]    0   50  100  150  200  250  300  350  400  450  500  550  600  650  700
#> [16]  750  800  850  900  950 1000
```
