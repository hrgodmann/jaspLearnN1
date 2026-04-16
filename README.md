# The Learn N=1 Module

## Overview

The JASP Learn N=1 module is an add-on module for JASP that introduces single-subject (N=1) analysis techniques through interactive tutorials with built-in simulations. The module helps clinicians and researchers understand how to analyze intensive longitudinal data from a single individual, covering treatment evaluation, time series forecasting, and symptom network modeling. Each analysis includes introductory text, simulation-based learning, and the option to load real data.

## R Packages

The module relies on several R packages for its statistical computations:

- **nlme** — Linear mixed-effects models with AR(1) correlation for treatment evaluation ([nlme on CRAN](https://cran.r-project.org/package=nlme))
- **forecast** — Automatic ARIMA model selection and forecasting ([forecast on CRAN](https://cran.r-project.org/package=forecast))
- **tidygraph** / **ggraph** — Network construction and visualization ([tidygraph on CRAN](https://cran.r-project.org/package=tidygraph), [ggraph on CRAN](https://cran.r-project.org/package=ggraph))

## Analyses

The organization of the analyses within the Learn N=1 module in JASP is as follows:

```
--- Learn N=1
    - Does The Treatment Work?
    - How Do Symptoms Develop?
    - How Are Symptoms Connected?
```

## Key Features

**Does The Treatment Work?** Evaluates treatment effects using an interrupted time series design. Fits a linear mixed-effects model with phase-by-time interactions and AR(1) residual correlation to test whether symptom levels and trends changed across treatment phases (e.g., pre-treatment, treatment, post-treatment).

**How Do Symptoms Develop?** Models the temporal dynamics of symptoms using ARIMA models. Automatically selects the best-fitting ARIMA specification via `forecast::auto.arima` and supports forecasting future symptom trajectories with configurable forecast horizons and visualization options.

**How Are Symptoms Connected?** Constructs directed symptom networks where nodes represent problems and edges represent their temporal connections. Computes in-degree and out-degree centrality to identify the most influential symptoms, with customizable network visualizations across multiple time points.

## Maintainer

- Henrik Godmann
