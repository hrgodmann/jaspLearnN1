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

**How Are Symptoms Connected?** Visualizes perceived causal networks in which nodes represent selected problems and directed edges represent rated relationships. Connection summaries show severity separately from incoming and outgoing absolute strengths and signed sums, with explicit explanations of cancellation. These descriptive summaries support case formulation rather than determine treatment priorities. Networks can be recorded at multiple assessment occasions.

Network ratings support configurable display scales, numeric entry, midpoint markers and optional incoming/outgoing connection counts. Editable starting lists and JSON presets reuse problem definitions without patient ratings. New severity values remain explicitly unrated until confirmed. Each assessment can be cleared separately, and CSV/preset files are written only by their save buttons. See the [Network help](inst/help/Network.md) for scale interpretation, preset replacement and export details.

## Maintainer

- Henrik Godmann
