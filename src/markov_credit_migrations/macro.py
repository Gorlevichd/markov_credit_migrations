"""Probit models for mapping macroeconomic drivers to latent stress scores."""

from dataclasses import dataclass

import numpy as np
import pandas as pd
from scipy.stats import norm


@dataclass
class ProbitScoreModel:
    """Fitted probit model with the notebook's standardized latent score."""

    fitted_model: object
    mean: float
    standard_deviation: float

    @classmethod
    def fit(cls, default_indicator: np.ndarray, driver: np.ndarray) -> "ProbitScoreModel":
        """Fit a probit model without adding an implicit intercept."""

        from statsmodels.discrete.discrete_model import Probit

        endog = np.asarray(default_indicator, dtype=float)
        exog = np.asarray(driver, dtype=float).reshape(-1, 1)
        if endog.ndim != 1 or len(endog) != len(exog):
            raise ValueError("default_indicator and driver must have equal length")
        if len(endog) < 2 or not np.isin(endog, [0, 1]).all():
            raise ValueError("default_indicator must contain at least two binary observations")
        fitted = Probit(endog, exog).fit(disp=False)
        scores = cls._latent_scores(fitted.predict(exog))
        standard_deviation = float(scores.std(ddof=1))
        if not np.isfinite(standard_deviation) or standard_deviation == 0:
            raise ValueError("the fitted scores have zero or invalid variation")
        return cls(fitted, float(scores.mean()), standard_deviation)

    @staticmethod
    def _latent_scores(probabilities: np.ndarray) -> np.ndarray:
        clipped = np.clip(np.asarray(probabilities, dtype=float), 1e-12, 1 - 1e-12)
        return norm.ppf(clipped)

    def predict_score(self, driver: np.ndarray | pd.Series) -> np.ndarray:
        """Predict standardized latent scores for new driver observations."""

        exog = np.asarray(driver, dtype=float).reshape(-1, 1)
        probabilities = self.fitted_model.predict(exog)
        raw_scores = self._latent_scores(probabilities)
        return (raw_scores - self.mean) / self.standard_deviation


def annual_driver_change(
    frame: pd.DataFrame,
    *,
    date_column: str,
    value_column: str,
) -> pd.Series:
    """Aggregate an observation series by year and return percentage changes."""

    if date_column not in frame or value_column not in frame:
        raise ValueError(f"frame must contain {date_column!r} and {value_column!r}")
    dates = pd.to_datetime(frame[date_column], errors="raise").dt.to_period("Y")
    annual = frame.assign(_year=dates).groupby("_year")[value_column].sum()
    return annual.pct_change().dropna()

