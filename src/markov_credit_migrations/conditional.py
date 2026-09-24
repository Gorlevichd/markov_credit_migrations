"""Conditional migration matrices driven by a latent macroeconomic score."""

import numpy as np
from pydantic import BaseModel, Field
from scipy.stats import norm

from .ratings import Rating


class ConditionalModelConfig(BaseModel):
    """Dependence parameters for investment and speculative grades."""

    gamma_invest: float = Field(ge=0, lt=1)
    gamma_speculative: float = Field(ge=0, lt=1)


def conditional_transition_matrix(
    baseline: np.ndarray,
    latent_score: float,
    config: ConditionalModelConfig,
) -> np.ndarray:
    """Apply a one-factor Gaussian stress adjustment to a baseline matrix.

    Rows represent the current rating and columns represent the next rating.
    The returned matrix remains row-stochastic up to floating-point tolerance.
    """

    matrix = np.asarray(baseline, dtype=float)
    expected_shape = (len(Rating), len(Rating))
    if matrix.shape != expected_shape:
        raise ValueError(f"baseline must be a {expected_shape[0]}x{expected_shape[1]} matrix")
    if not np.isfinite(matrix).all() or (matrix < 0).any():
        raise ValueError("baseline must contain finite, non-negative probabilities")
    if not np.allclose(matrix.sum(axis=1), 1.0):
        raise ValueError("each baseline row must sum to one")
    if not np.isfinite(latent_score):
        raise ValueError("latent_score must be finite")

    gammas = np.full(len(Rating), config.gamma_speculative)
    gammas[: int(Rating.BB_B)] = config.gamma_invest
    cumulative = np.cumsum(matrix[:, ::-1], axis=1)
    quantiles = norm.ppf(np.clip(cumulative, 0, 1))
    quantiles[:, -1] = np.inf
    scaled = (quantiles - gammas[:, None] * latent_score) / np.sqrt(1 - gammas[:, None] ** 2)
    conditional_cdf = norm.cdf(scaled)
    result = np.diff(conditional_cdf, axis=1, prepend=np.zeros((len(Rating), 1)))[:, ::-1]
    result = np.clip(result, 0, None)
    return result / result.sum(axis=1, keepdims=True)

