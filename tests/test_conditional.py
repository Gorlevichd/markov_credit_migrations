import numpy as np

from markov_credit_migrations.conditional import (
    ConditionalModelConfig,
    conditional_transition_matrix,
)


def test_conditional_matrix_is_row_stochastic():
    baseline = np.full((5, 5), 0.1)
    result = conditional_transition_matrix(
        baseline,
        latent_score=1.0,
        config=ConditionalModelConfig(gamma_invest=0.2, gamma_speculative=0.5),
    )
    np.testing.assert_allclose(result.sum(axis=1), 1.0)
    assert (result >= 0).all()


def test_zero_dependence_preserves_baseline():
    baseline = np.eye(5)
    result = conditional_transition_matrix(
        baseline,
        latent_score=-2.0,
        config=ConditionalModelConfig(gamma_invest=0, gamma_speculative=0),
    )
    np.testing.assert_allclose(result, baseline)

