# missalpha

Calculate the maximum and minimum Cronbach's alpha with a dataset with missing data. 

# Installation

The R package ``missalpha`` is publicly available on [Github](https://github.com/Feng-Ji-Lab/missalpha) (latest development version):

## Github
``` r
devtools::install_github("Feng-Ji-Lab/missalpha")
library(missalpha)
```

The CRAN version is incoming soon.

# Functionality

`missalpha` provides the following main functionalities:

- `cronbachs_alpha()`: Unified wrapper function for computing alpha bounds via different methods.
- `compute_alpha_min()` / `compute_alpha_max()`: Core functions using binary search with optimization (e.g., GA, DEoptim, nloptr) to solve for alpha bounds.
- `cronbach_alpha_enum()`: Exhaustive enumeration of all missing value configurations for exact bound computation.
- `cronbach_alpha_rough()`: Monte Carlo approximation of alpha bounds for large-scale problems.
- `display_all()`: Function to compare and visualize results across all methods.

Internally, all methods formulate the alpha bound problem as a constrained nonlinear program and apply black-box solvers from `GA`, `DEoptim`, and `nloptr`. These solvers identify imputations of missing entries that minimize or maximize the alpha value, thus constructing the global worst-case bounds.

# Examples

To illustrate the usage of `missalpha`, we provide several examples demonstrating different methods to compute bounds on Cronbach’s alpha under missing data:

```r
  scores_df <- missalpha::sample
  scores_mat <- as.matrix(scores_df)
  result <- cronbachs_alpha(scores_mat, 4, enum_all = FALSE)
  summary(result)
```

The results are shown below:

```
> head(scores_df)
  V1 V2 V3 V4
1 NA  1  0  0
2  0  0  0  0
3 NA  0  0  0
4  2  0  0  1
5 NA  0  0  0
6  0  0  0  0

>   summary(result)
Summary of Cronbach's Alpha Bounds Calculation:

Optimization Method: GA
Alpha Min (Optimized): 0.000488
Alpha Max (Optimized): 0.403809

Runtime Information:
Total Runtime: 17.165619 seconds
```

In this example, we use a sample dataset (`missalpha::sample`) containing 50 individuals and 4 items with missing values. The item scores range from 0 to 4. The optimization-based method (`cronbachs_alpha()`) was applied using the default genetic algorithm (GA) with a score maximum of 4.

The estimated bounds for Cronbach’s alpha were `[0.000, 0.404]`, indicating a wide range of uncertainty in the internal consistency of the scale.

The total runtime of approximately 17 seconds reflects the computational cost of performing constrained optimization over all plausible missing value completions.

To further demonstrate the types of datasets that `missalpha` can handle, we generate a synthetic matrix with missing values using a Bernoulli process. This simulates a common testing scenario where some item responses are randomly missing across individuals. The matrix contains responses (0/1/2), and 20 entries out of the 500 entries are randomly set to missing (NA).

``` r
set.seed(0)
score_max <- 2
scores_mat_bernoulli <- generate_scores_mat_bernoulli(
  n_person = 50,
  n_item = 10,
  n_missing = 20,
  score_max = score_max
)

result = cronbachs_alpha(
    scores_mat_bernoulli, score_max, enum_all = FALSE
)
summary(result)
```

The result is shown as:

```
> summary(result)
Summary of Cronbach's Alpha Bounds Calculation: 

Optimization Method: GA
Alpha Min (Optimized): 0.762207
Alpha Max (Optimized): 0.817871

Runtime Information:
Total Runtime: 19.001663 seconds
```

We also provide a function that calculates all estimates and display them:

```r
  all_result = display_all(scores_mat = scores_mat,score_max = 2)
  summary(all_result)
```

The results are shown below:

```
>   summary(all_result)
Rough_Integer_Method:
Alpha Min: 0.201523
Alpha Max: 0.392180
Runtime: 0.084263 seconds

Rough_Float_Method:
Alpha Min: 0.217747
Alpha Max: 0.392180
Runtime: 0.086584 seconds

Optimization_Method_GA:
Alpha Min: 0.194824
Alpha Max: 0.404785
Runtime: 16.930677 seconds

Optimization_Method_DEoptim:
Alpha Min: 0.192871
Alpha Max: 0.404785
Runtime: 1.099646 seconds

Optimization_Method_nloptr:
Alpha Min: 0.191895
Alpha Max: 0.404785
Runtime: 0.029727 seconds
```

For more information, please refer to [our manual](missalpha_0.1.0.pdf).
