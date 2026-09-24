"""Continuous-time Markov transition-matrix estimation."""

from dataclasses import dataclass

import numpy as np
import pandas as pd
from scipy.linalg import expm

from .ratings import Rating


@dataclass(frozen=True)
class TransitionEstimate:
    """Estimated generator and one-year transition probabilities."""

    generator: np.ndarray
    probabilities: np.ndarray
    changes: np.ndarray
    exposure_days: np.ndarray


def _validate_observations(frame: pd.DataFrame) -> None:
    required = {"company_name", "date", "rating"}
    missing = required.difference(frame.columns)
    if missing:
        raise ValueError(f"Missing required columns: {sorted(missing)}")
    if frame.empty:
        raise ValueError("At least one rating observation is required")


def estimate_transition_matrix(
    frame: pd.DataFrame,
    *,
    end_date: pd.Timestamp | None = None,
) -> TransitionEstimate:
    """Estimate a generator from dated company rating observations.

    Exposure is accumulated in the current state until the next observation;
    an observed migration contributes one count from the old state to the new
    state. The resulting generator is exponentiated over one year.
    """

    _validate_observations(frame)
    states = len(Rating)
    changes = np.zeros((states, states), dtype=float)
    exposure_days = np.zeros(states, dtype=float)
    end = end_date or pd.Timestamp.now(tz="UTC").tz_localize(None)

    for _, company in frame.sort_values("date").groupby("company_name"):
        observations = company.sort_values("date")
        for previous, current in zip(
            observations.iloc[:-1].itertuples(), observations.iloc[1:].itertuples()
        ):
            old = int(previous.rating)
            new = int(current.rating)
            days = max((current.date - previous.date).days, 0)
            exposure_days[old] += days
            if old != new:
                changes[old, new] += 1
        if len(observations) == 1:
            only = observations.iloc[0]
            exposure_days[int(only.rating)] += max((end - only.date).days, 0)

    years = np.maximum(exposure_days / 365.0, 1e-5)
    years[int(Rating.D)] = 1.0
    generator = changes / years[:, None]
    np.fill_diagonal(generator, -generator.sum(axis=1))
    probabilities = expm(generator)
    return TransitionEstimate(generator, probabilities, changes, exposure_days)


def transition_probabilities(
    probabilities: np.ndarray,
    rating: int | Rating,
    *,
    rng: np.random.Generator | None = None,
) -> Rating:
    """Sample a next rating from one row of a transition matrix."""

    matrix = np.asarray(probabilities, dtype=float)
    if matrix.shape != (len(Rating), len(Rating)):
        raise ValueError("probabilities must be a 5x5 matrix")
    state = int(rating)
    if state not in range(len(Rating)):
        raise ValueError(f"rating must be between 0 and {len(Rating) - 1}")
    row = np.clip(matrix[state], 0, None)
    total = row.sum()
    if not np.isclose(total, 1.0):
        if total <= 0:
            raise ValueError("the selected transition row has no probability mass")
        row = row / total
    generator = rng or np.random.default_rng()
    return Rating(generator.choice(len(Rating), p=row))
