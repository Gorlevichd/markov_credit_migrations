# Markov Credit Migrations

Python tools for modelling changes in S&P credit ratings with continuous-time
Markov transition matrices and macroeconomic conditional probabilities.

## Installation

```bash
python -m pip install -e ".[dev]"
```

The package expects Python 3.12 or newer.

## Rating normalization

Raw Bloomberg/S&P rating notches are collapsed into the five model states:

```python
from markov_credit_migrations import Rating, normalize_rating

assert normalize_rating("AA-") is Rating.AAA_A
assert normalize_rating("BBB+") is Rating.BBB
assert normalize_rating("Bpi") is Rating.BB_B
assert normalize_rating("SD") is Rating.D
assert normalize_rating("ND") is None
```

## Estimate a transition matrix

The estimator expects one row per company-rating observation, with dates sorted
within each company. The matrix rows represent the current rating and columns
represent the next rating.

```python
import pandas as pd

from markov_credit_migrations import estimate_transition_matrix

observations = pd.DataFrame(
    {
        "company_name": ["Alpha", "Alpha", "Beta", "Beta"],
        "date": pd.to_datetime(
            ["2020-01-01", "2021-01-01", "2020-01-01", "2021-01-01"]
        ),
        "rating": [1, 2, 0, 0],  # BBB -> BB-B and AAA-A -> AAA-A
    }
)

estimate = estimate_transition_matrix(
    observations,
    end_date=pd.Timestamp("2021-01-02"),
)

print(estimate.probabilities)
print("BBB to BB-B events:", estimate.changes[1, 2])
```

For raw Bloomberg-style exports, use the preprocessing helper first:

```python
from markov_credit_migrations.preprocessing import prepare_ratings

prepared = prepare_ratings(raw_frame)
estimate = estimate_transition_matrix(prepared)
```

## Apply a conditional migration matrix

The conditional model shifts a baseline matrix using a standardized latent
stress score. Investment-grade states (`AAA-A`, `BBB`) and speculative-grade
states use separate dependence parameters.

```python
import numpy as np

from markov_credit_migrations import (
    ConditionalModelConfig,
    conditional_transition_matrix,
)

baseline = np.eye(5)
config = ConditionalModelConfig(
    gamma_invest=0.20,
    gamma_speculative=0.50,
)

stressed = conditional_transition_matrix(
    baseline,
    latent_score=1.0,
    config=config,
)

assert stressed.shape == (5, 5)
assert np.allclose(stressed.sum(axis=1), 1.0)
```

## Fit a probit stress-score model

The probit model maps a binary default indicator and an economic driver, such
as annual GDP growth or CDS changes, to a standardized latent score.

```python
import numpy as np

from markov_credit_migrations import ProbitScoreModel

rng = np.random.default_rng(7)
driver = rng.normal(size=200)
default_probability = 1 / (1 + np.exp(-0.8 * driver))
default_indicator = rng.binomial(1, default_probability)

model = ProbitScoreModel.fit(default_indicator, driver)
future_driver = np.array([-1.0, 0.0, 1.0])
future_scores = model.predict_score(future_driver)
print(future_scores)
```

The score can be passed to `conditional_transition_matrix` to produce a
scenario-specific transition matrix.

## Data privacy

Source rating exports and economic datasets are not part of the public Python
package. Keep private data outside version control and pass it into the
preprocessing functions as a `pandas.DataFrame`.
