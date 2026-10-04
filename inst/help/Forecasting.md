# Forecasting

Use an ARIMA model to explore how an outcome might develop if its historical relationships continue. A forecast alone does not establish treatment effectiveness or justify a treatment decision. Choose a regular measurement interval that is meaningful for the outcome and feasible for the client.

## Simulate or fit a series

Simulation returns exactly **N** observations. AR coefficients must define a stationary process; with one AR lag its magnitude must be below 1. For multiple lags, stationarity depends on the coefficients jointly. Differencing adds integrated behavior to that stationary process. The noise standard deviation is nonnegative; zero produces a deterministic demonstration, which need not support coefficient inference. N must be at least 2, but this minimum is not a claim that such a short series gives reliable estimates.

**Automatic** specification uses the selected AICc, AIC or BIC criterion to compare candidate models, with KPSS tests as the default nonseasonal differencing rule. **Manual** specification uses the chosen p, d and q. The existing manual convention includes a mean when d = 0, drift when d = 1, and no constant when d > 1. The fitted-model note states the selected orders and whether a mean or drift was included.

Without covariates, **Mean** is the undifferenced process mean. With covariates, **Intercept** is the regression intercept, accounting for ARIMA errors. **Drift** describes a linear trend. Coefficient standard errors come from the fitted model; p-values and confidence intervals use an approximate t reference with used observations minus estimated coefficients as degrees of freedom. This is not a validated small-sample correction. The results condition on the fitted model and do not adjust for automatic model selection.

## Interpret forecasts and intervals

The forecast table and plot show 80% and 95% prediction intervals for future observations. These standard intervals condition on the fitted model: parameter and model-selection uncertainty are not included. They also assume that the modeled relationships continue. With covariates, uncertainty about the supplied future predictor values is omitted too. Check whether residual patterns or changes in circumstances make the model unsuitable; a narrow interval is not proof of forecast accuracy.

The maximum horizon is 10000 measurement steps to limit resource use. It is not a statistical reliability threshold, and a shorter horizon may be more useful. A saved request above the limit gives a message rather than silently truncating its forecasts.

## Forecasting with predictors

Forecasting fits a regression with ARIMA errors when covariates are selected. The regression describes associations between the outcome and the supplied numeric predictors; ARIMA describes the remaining serial dependence. Both automatic and manual model specification support covariates. Automatic specification chooses the ARIMA order while retaining the selected predictors.

## Prepare the historical observations

Assign the outcome and, optionally, a numeric time column. Measurement occasions must be equally spaced. Rows are sorted by the assigned time; without a time column, row order defines consecutive steps. Keep a row for a missing outcome rather than removing its time position.

Fractional time units are supported. If very large absolute timestamps prevent reliable distinction between consecutive occasions, express time relative to the start of the series. Requested future times must continue the same grid; a forecast step always means one measurement interval.

Select numeric covariates measured on those occasions. Every historical covariate value must be finite and complete, including rows with a missing outcome. The outcome cannot also be a covariate. Constant or redundant predictors cannot be estimated separately. Differencing can also make predictors redundant, including a linear time predictor that duplicates the model's drift term.

The coefficient table contains only coefficients estimated by the model. Covariate coefficients express conditional associations, not evidence that changing a predictor causes the outcome to change.

## Supply a future scenario

Model estimation and coefficient reporting do not require future predictor values. Forecasting with predictors does: append rows after the last observed outcome, leave the outcome cells empty, and fill in every selected predictor. If a time column is assigned, also fill in its equally spaced future values.

For example, to forecast two future measurements using stress as a predictor:

| Time | Symptom score | Stress |
| --- | --- | --- |
| 1 | 12 | 4 |
| 2 | 15 | 6 |
| ... | ... | ... |
| 30 | 9 | 3 |
| 31 | *(empty)* | 3 |
| 32 | *(empty)* | 2 |

Set **Number of forecasts** to 2 and enable the forecast table or plot. Rows 31 and 32 provide the future predictor scenario and are excluded from estimation. Extra future rows are ignored beyond the requested horizon. Missing or insufficient future predictor values produce a forecast-specific message; the fitted coefficients remain available.

Future predictor values can be known in advance or chosen as an explicit scenario. The module does not forecast the predictors automatically. The forecast means and prediction intervals are conditional on the supplied scenario; they do not include uncertainty about those future predictor values.

For multiple scenarios, change the future predictor rows and compare the resulting forecasts. Changing only future predictor values should leave the estimated historical coefficients unchanged.

Without covariates, forecasts use the outcome's history alone and do not require appended future rows. In either case, forecasting starts immediately after the last observed outcome.

## Export a CSV

Choose a destination, then press **Export CSV / Save again**. Selecting a path, editing data or options, and reopening an analysis do not authorize a write. The button replaces an existing file at the selected destination. The export status reports whether the current forecasts were saved or whether changes are pending.

If an export fails, correct the input or destination and press the button again. Repairing a folder or forecast scenario does not retry automatically. The coefficient and forecast outputs remain available when a file write fails. In sandboxed JASP builds, use an accessible destination offered by the host file chooser.

## References

- [forecast: ARIMA estimation with external regressors](https://pkg.robjhyndman.com/forecast/reference/Arima.html)
- [forecast: automatic ARIMA selection](https://pkg.robjhyndman.com/forecast/reference/auto.arima.html)
- [forecast: future regressor values and prediction intervals](https://pkg.robjhyndman.com/forecast/reference/forecast.Arima.html)
- [Forecasting: Principles and Practice — regression with ARIMA errors](https://otexts.com/fpp3/regarima.html)
- [Forecasting: Principles and Practice — ARIMA prediction intervals](https://otexts.com/fpp3/arima-forecasting.html)
