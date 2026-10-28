# Stan models

- `model_ai.survey_structured.stan`: baseline single-wave model, updated for current Stan syntax.
- `model_ai.survey_structured_optimised.stan`: optimised single-wave model, used for the individual-wave configurations.
- `model_ai.survey_structured_temporal.stan`: temporal model retained from the original analysis.
- `model_ai.survey_structured_temporal_optimised.stan`: optimised temporal model, used for `pooled_high` and `pooled_moderate`.
- `functions.stan`: shared functions used by the models.
- `notes_on_optimised_model.pdf`: explanation of the optimisation changes.

Single-wave configurations fit one survey wave at a time. Pooled configurations combine all five survey waves, retain each record's wave label, and fit the optimised temporal model. Their poststratified predictions refer to the latest wave, informed by all five waves.

Keep `functions.stan` in the same directory as the Stan models.