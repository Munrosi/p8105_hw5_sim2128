HW 5
================
Sarah Munro
11/5/2019

# Problem 1

Create a function that replaces missing numeric values with the column
mean, and replace missing character variables with “virginica”, then map
it over the dataset.

``` r
replace_x = function(x) {
  if (is.numeric(x) == T) {
  x[is.na(x)] = (mean(x, na.rm = T))}
  else {
   x[is.na(x)] = "virginica" }
  x
}
iris_with_missing = map_df(iris_with_missing, replace_x)
```
