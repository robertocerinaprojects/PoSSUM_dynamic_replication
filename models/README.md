# Stan models

- `model_ai.survey_structured.stan`: baseline single-wave model, updated for current Stan syntax.
- `model_ai.survey_structured_optimised.stan`: computationally optimised version of the same statistical model, used by the current replication workflow.
- `model_ai.survey_structured_temporal.stan`: temporal model retained from the original analysis.
- `functions.stan`: shared functions used by the models.
- `notes_on_optimised_model.pdf`: explanation of the optimisation changes.

Keep `functions.stan` in the same directory as the Stan models.
The current workflow fits the single-wave model.