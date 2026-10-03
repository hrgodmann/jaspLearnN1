# Forecasting with predictors

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

## References

- [forecast: ARIMA estimation with external regressors](https://pkg.robjhyndman.com/forecast/reference/Arima.html)
- [forecast: automatic ARIMA selection](https://pkg.robjhyndman.com/forecast/reference/auto.arima.html)
- [forecast: future regressor values and prediction intervals](https://pkg.robjhyndman.com/forecast/reference/forecast.Arima.html)
- [Forecasting: Principles and Practice — regression with ARIMA errors](https://otexts.com/fpp3/regarima.html)
