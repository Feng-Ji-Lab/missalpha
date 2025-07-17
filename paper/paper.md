
---
title: '``missalpha``: An R package for computing bounds of Cronbach''s alpha with missing data'
authors:
- name: Feng Ji
  orcid: 0000-0002-2051-5453
  affiliation: 1
  corresponding: true
  email: f.ji@utoronto.ca
- name: Biying Zhou
  orcid: 0000-0002-3590-3408
  affiliation: 1
affiliations:
- name: Department of Applied Psychology & Human Development, University of Toronto, Toronto, Canada
  index: 1
date: "\\today"
output: pdf_document
bibliography: paper.bib
tags:
- R
- Cronbach alpha
- Manski bound
- Reliability
---

# Summary

Cronbach's alpha is a widely used index of internal consistency and scale reliability in psychological and educational measurement. Despite its popularity, standard implementations often fail to account for missing data appropriately, leading researchers to either use ad-hoc methods or rely on listwise deletion. In practice, this can result in biased reliability estimates.

To address this, we developed `missalpha`, an R package that estimates the upper and lower bounds of Cronbach's alpha under arbitrary missingness mechanisms. Our approach is inspired by the concept of *Manski bounds* [@manski2003partial], offering researchers a robust, agnostic summary of reliability when the missing data mechanism is unknown or not easily modeled. `missalpha` implements both exact enumeration (for small problems) and optimization-based algorithms (for larger datasets), enabling principled worst-case scenario analysis for reliability.

# Statement of Need

In applied research, Cronbach's alpha is often reported as a point estimate and compared against conventional thresholds (e.g., 0.7 or 0.8) to judge scale adequacy [@nunnally1978psychometric]. However, in the presence of missing data, particularly when the missingness mechanism is unclear, standard point estimation may over- or under-estimate the true internal consistency of a scale.

Existing packages like `psych` [@revelle2017psych] and `ltm` [@rizopoulos2007ltm] compute alpha but assume complete data or impute missing entries without evaluating uncertainty in reliability caused by missingness. To our knowledge, no current package offers a general framework to compute bounds on Cronbach's alpha that remain valid under arbitrary missing data patterns.

The `missalpha` package fills this gap by providing tools to:
- Compute sharp lower and upper bounds of Cronbach's alpha under any missing data mechanism;
- Perform sensitivity analysis via enumeration, Monte Carlo approximation, and global optimization;
- Support both discrete (Likert-type) and continuous response formats.

The package is useful when researchers seek to evaluate how missing data may affect conclusions about scale reliability, and when no strong assumptions about the missingness mechanism can be made.


# Package Features

`missalpha` provides the following main functionalities:

- `cronbachs_alpha()`: Unified wrapper function for computing alpha bounds via different methods.
- `compute_alpha_min()` / `compute_alpha_max()`: Core functions using binary search with optimization (e.g., GA, DEoptim, nloptr) to solve for alpha bounds.
- `cronbach_alpha_enum()`: Exhaustive enumeration of all missing value configurations for exact bound computation.
- `cronbach_alpha_rough()`: Monte Carlo approximation of alpha bounds for large-scale problems.
- `display_all()`: Function to compare and visualize results across all methods.

Internally, all methods formulate the alpha bound problem as a constrained nonlinear program and apply black-box solvers from `GA` [@scrucca2013ga], `DEoptim` [@mullen2011deoptim], and `nloptr` [@ypma2018package]. These solvers identify imputations of missing entries that minimize or maximize the alpha value, thus constructing the global worst-case bounds.

# Availability

The R package ``missalpha`` is publicly available on [Github](https://github.com/Feng-Ji-Lab/missalpha) (latest development version):

## Github
``` r
devtools::install_github("Feng-Ji-Lab/missalpha")
library(missalpha)
```


# References