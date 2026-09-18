# Get limits of numeric vector

This function calculates the limits of a numeric vector and returns them
after rounding to specified number of significant digits.

## Usage

``` r
get_limits(gq, sig_digits = 3, na.rm = TRUE)
```

## Arguments

- gq:

  numeric vector

- sig_digits:

  significant digits for rounding

- na.rm:

  remove NAs? Default: TRUE

## Value

vector of length 2 with min, max
