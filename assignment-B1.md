assignment-B1
================
Hasti Jalali - 55543185

``` r
library(testthat)
library(tibble)
```

## Functions

In the following block, I used roxygen2 tags to document the function.
These tags describe what the function does, explain each of its
arguments, and show how to use it through the @examples section. This
helps make the code easier to understand and reuse later.

``` r
#' Summarize a Linear Relationship
#'
#' Fits a simple linear regression between two numeric variables and returns
#' the slope, intercept, and R-squared in a tidy tibble.
#'
#' @param data A data frame that holds the variables. I called it `data` 
#' because it’s a clear name for the dataset input.
#' @param x Name of the predictor variable. I used `x` because
#' it usually represents the independent variable in most formulas.
#' @param y Name of the response variable. I used `y` because
#' it commonly represents the dependent variable in regression.
#'
#' @return A tibble with columns for predictor, response, slope, intercept, and r_squared.
#'
#' @examples
#' linear_summary(mtcars, "hp", "mpg")
#' linear_summary(iris, "Sepal.Length", "Petal.Length")
linear_summary <- function(data, x, y) {
  
  if (!all(c(x, y) %in% names(data))) {
    stop("Columns not found.")
  }
  
  if (!is.numeric(data[[x]]) || !is.numeric(data[[y]])) {
    stop("Columns must be numeric.")
  }
  
  model <- lm(data[[y]] ~ data[[x]])
  summary_info <- summary(model)
  
  tibble::tibble(
    predictor = x,
    response = y,
    slope = unname(coef(model)[2]),
    intercept = unname(coef(model)[1]),
    r_squared = unname(summary_info$r.squared)
  )
}
```

The function returns a tidy tibble with one row that summarizes the
fitted linear model. It includes the predictor and response variable
names, the estimated slope and intercept from the regression, and the
R-squared value, which shows how well the model fits the data. A higher
`r_squared` is better, because it means the model’s predictions are
closer to the actual data points.

## Examples

In the following three blocks, we show how the function works with
different datasets and inputs.

``` r
linear_summary(mtcars, "hp", "mpg")
```

    ## # A tibble: 1 × 5
    ##   predictor response   slope intercept r_squared
    ##   <chr>     <chr>      <dbl>     <dbl>     <dbl>
    ## 1 hp        mpg      -0.0682      30.1     0.602

From this output, we can see that `mpg` decreases as `hp` increases,
with a negative slope. Since the r-squared value is around 0.6, it means
the model explains about 60% of the variation in fuel efficiency, which
shows a moderately strong relationship.

``` r
linear_summary(iris, "Sepal.Length", "Petal.Length")
```

    ## # A tibble: 1 × 5
    ##   predictor    response     slope intercept r_squared
    ##   <chr>        <chr>        <dbl>     <dbl>     <dbl>
    ## 1 Sepal.Length Petal.Length  1.86     -7.10     0.760

Here, both variables have a positive slope, showing that flowers with
longer sepals also tend to have longer petals. The r-squared value is
high (around 0.76), meaning the model fits the data quite well.

``` r
linear_summary(iris, "Species", "Sepal.Length")
```

    ## Error in linear_summary(iris, "Species", "Sepal.Length"): Columns must be numeric.

This example gives an **error**, because the `Species` column is not
numeric.

## Testing

In this part, I created three small datasets to make sure the function
works as expected.

- The first dataset has a perfect linear relationship, so the test
  checks that the `slope`, `intercept`, and `r-squared` values are
  exactly what they should be.

- The second dataset includes an NA value to confirm the function still
  gives consistent results when there are missing data.

- The third dataset contains a non-numeric column to test error
  handling.

``` r
# Perfect linear relationship
df1 <- tibble(x = 1:5, y = 2 * x + 1) # slope=2, intercept=1

# With an NA
df2 <- tibble(x = c(1, 2, 3, 4, NA), y = c(2, 4, 6, 8, 10))

# With a non-numeric column
df3 <- tibble(x = c("a", "b", "c"), y = c(1, 2, 3))
```

``` r
test_that("linear_summary works correctly on a perfect linear relationship", {
  result <- suppressWarnings(linear_summary(df1, "x", "y"))
  
  expect_equal(result$slope, 2, tolerance = 1e-8)
  expect_equal(result$intercept, 1, tolerance = 1e-8)
  expect_equal(result$r_squared, 1, tolerance = 1e-8) #r_squared should be 1 since it's predecting prefectly
})
```

    ## Test passed 😸

``` r
test_that("linear_summary handles NA values properly", {
  result <- suppressWarnings(linear_summary(df2, "x", "y"))
  
  df2_clean <- df2[complete.cases(df2), ]
  expected <- suppressWarnings(linear_summary(df2_clean, "x", "y"))

  expect_equal(result$slope, expected$slope, tolerance = 1e-8)
  expect_equal(ncol(result), 5)
  expect_s3_class(result, "tbl_df")
})
```

    ## Test passed 😸

``` r
test_that("linear_summary throws an error for non-numeric input", {
  expect_error(linear_summary(df3, "x", "y"), "must be numeric")
})
```

    ## Test passed 😀
