# Treatment: compare phases for one individual

The analysis estimates phase-specific linear trends with AR(1) residual correlation for one person's series, using generalized least squares (GLS) with restricted maximum likelihood (REML). The intercept and phase-specific trends describe this individual. No between-person random-intercept variance is estimated. For loaded data, assign three distinct columns: a numeric outcome, a numeric time variable, and a phase variable.

## Phase comparisons

Use **Phase comparisons** to choose a **Compared phase** and a **Reference phase**. The automatic choices are the second and first phases in chronological order, respectively, regardless of alphabetical order or the regression's reference category. The output always names both phases. Explicit choices are kept until changed; if a selected phase is unavailable, select it again.

All differences are **compared phase minus reference phase**:

- **Endpoint difference:** the fitted outcome at the compared phase's own last scheduled time minus the fitted outcome at the reference phase's own last scheduled time. Both endpoint times appear beneath the table. This is a comparison of fitted endpoints at two different times, not an immediate change at treatment onset and not a difference between raw phase averages.
- **Slope difference:** the difference between rates of change moving forward, per unit of the selected time variable. Negative means the compared phase has a more negative slope; whether that is improvement depends on the outcome scale.

For example, with baseline at times 1–20 and treatment at 21–40, Treatment minus Baseline compares the fitted treatment outcome at time 40 with the fitted baseline outcome at time 20. It does not compare the two lines at time 21. If the outcome at time 40 is missing but its scheduled row remains, the endpoint is still evaluated at time 40; uncertainty comes from the fitted model. It may require extrapolation beyond the last observed outcome.

**Phase estimates** optionally shows each phase's fitted endpoint and forward slope, with confidence intervals. All estimates and comparisons use the same fitted GLS model, including its full coefficient covariance. Selecting another comparison does not change the fitted lines or residual autocorrelation. The confidence-level control applies to coefficients, phase estimates, comparisons, and autocorrelation intervals.

Each compared or summarized phase must be one continuous period. For an A–B–A design, use distinct episode names such as Baseline 1, Treatment, and Baseline 2. Repeated episode labels cannot silently be interpreted as a single endpoint; the affected table asks you to rename them. Other model outputs remain available. More than two phases can be fitted jointly, and a single pair can be selected for comparison.

### Relation to the published N=1 approach

[Wagemaker et al. (2017), pp. 5–6](https://doi.org/10.1080/19315864.2017.1320601) compare each phase's own fitted endpoint and phase-specific trends. They reverse time within each phase so that its end is zero. Our endpoint contrasts answer that same question without changing the fitted model or the input time values. Slopes are reported in forward time, so their signs must not be compared directly with coefficients on a reversed time scale. [Maric et al. (2015), p. 234](https://doi.org/10.1016/j.beth.2014.09.005) describe REML estimation with `nlme::gls` for this framework.

### Interpretation

Consider endpoint and slope differences together. A better treatment endpoint can reflect an improvement trend that already existed before treatment. Phase-associated differences alone do not establish that treatment caused the change, and a small p-value does not establish clinical importance. Judge the magnitude of change in the context of the outcome measure.

The comparison table reports two unadjusted tests, one for each question; it is not a single test of treatment success. Inspecting many pairs or selecting comparisons after examining results increases the scope of testing. This interface does not implement an all-pairs post hoc procedure or an omnibus test.

## Use one continuous clock

Time must be complete, unique and equally spaced across the full series. The analysis sorts rows by time before fitting and plotting. Do not restart time at the beginning of a new phase. Duplicate timestamps and irregularly spaced rows are rejected with a message explaining what to correct.

For example, weekly measurements can use times `0, 7, 14, 21, ...`, including when treatment begins. One AR(1) step then means seven time units. The Measurement timing output reports the inferred interval; it cannot determine whether the numeric units represent days, weeks or another unit. Without a known collection schedule, the module cannot detect that every other scheduled row has been removed.

The regression uses your actual time values. Changing units changes slope units; changing the origin changes the meaning of the raw intercept and phase coefficients. The explicitly calculated endpoint differences retain their meaning when the time origin changes. These transformations should leave fitted values and residual autocorrelation unchanged, apart from numerical precision. The phase reference category is not changed by sorting. The optional coefficient table identifies its reference coding and the meaning of regression time zero; its raw phase coefficients are not automatically endpoint or onset comparisons. Custom contrast coding is retained and identified when it has no single treatment-coded reference phase.

Prefer time since the series began to very large absolute timestamps. If the time coding makes the regression numerically unestimable, the analysis asks you to express time relative to the start, while retaining one continuous clock across phases.

## Keep missing measurements in the grid

Keep a row for each scheduled occasion. Fill in its time and phase, and leave the outcome empty if it was not measured:

| Time | Outcome | Phase |
| --- | --- | --- |
| 1 | 12 | Baseline |
| 2 | 10 | Baseline |
| 3 | *(empty)* | Baseline |
| 4 | 9 | Baseline |
| 5 | 8 | Treatment |

This illustrates the data format, not a sufficient sample for analysis. At least two phases are needed, each with at least two observed outcomes at different times, and there must be more than two observed outcomes per phase on average to estimate uncertainty. These are minimum requirements for estimation, not evidence that such a small sample gives reliable inference.

Missing outcomes are not imputed. Only observed outcomes contribute to estimation, but the autocorrelation uses their original scheduled positions. In this example, residual correlation between times 2 and 4 is `rho^2`, not `rho`. Correlation continues across phase boundaries. Missing time or phase values are rejected because the analysis cannot locate those observations reliably.

Plots retain the chronological positions. Missing outcomes have no plotted point or fitted value; lines break at internal missing values. A missing final outcome can therefore make the visible line end before the endpoint evaluated in the comparison table. This handling does not resolve bias from informative missingness, such as measurements being omitted because symptoms were unusually severe.

## Simulations use two time variables

To retain the simulation controls' existing interpretation, regression time starts at 1 within each phase. The plot and AR(1) errors use a separate continuous sequence across all phases. A phase change does not reset the residual process. Endpoint comparisons evaluate each phase at the end of its own regression clock, while displayed endpoint times use the continuous measurement sequence. Simulated slopes are per measurement occasion.

## Current scope

Coefficient and comparison tests and confidence intervals use the fitted GLS covariance and a t reference distribution with the number of observed outcomes minus the number of regression coefficients as degrees of freedom. Phase-estimate intervals use that same approximation. The autocorrelation interval uses a transformed normal approximation. These approximations do not establish reliable coverage in short series or with strong autocorrelation; no universal minimum number of observations guarantees reliable inference. Adding explicitly labelled phase comparisons does not resolve this separate limitation.

## Technical reference

[nlme's AR(1) correlation structure](https://stat.ethz.ch/R-manual/R-devel/library/nlme/html/corAR1.html) accepts an explicit integer time covariate. The module creates that index from the validated grid before excluding missing outcomes; `na.exclude` preserves fitted-value alignment with the original occasions.

[nlme's GLS implementation](https://stat.ethz.ch/R-manual/R-devel/library/nlme/html/gls.html) fits the regression with correlated residuals directly. It retains the individual's intercept without adding a random-intercept variance across artificial groups.
