import numpy as np
import pandas as pd

from markov_credit_migrations.ratings import Rating
from markov_credit_migrations.transitions import (
    estimate_transition_matrix,
    transition_probabilities,
)


def test_estimate_transition_matrix_has_stochastic_rows():
    observations = pd.DataFrame(
        {
            "company_name": ["A", "A", "B", "B"],
            "date": pd.to_datetime(["2020-01-01", "2021-01-01", "2020-01-01", "2021-01-01"]),
            "rating": [Rating.BBB, Rating.BB_B, Rating.AAA_A, Rating.AAA_A],
        }
    )
    result = estimate_transition_matrix(observations, end_date=pd.Timestamp("2021-01-02"))
    assert result.probabilities.shape == (5, 5)
    np.testing.assert_allclose(result.probabilities.sum(axis=1), 1.0)
    assert result.changes[Rating.BBB, Rating.BB_B] == 1


def test_transition_sampling_is_reproducible():
    probabilities = np.eye(5)
    assert transition_probabilities(probabilities, Rating.CCC_C) is Rating.CCC_C

