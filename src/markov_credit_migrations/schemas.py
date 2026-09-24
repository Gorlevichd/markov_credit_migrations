"""Validated data contracts used by the modelling code."""

from datetime import datetime

from pydantic import BaseModel, ConfigDict, Field

from .ratings import Rating


class RatingObservation(BaseModel):
    """One dated rating observation for a company."""

    model_config = ConfigDict(extra="forbid")

    company_name: str = Field(min_length=1)
    date: datetime
    rating: Rating
    industry_type: str | None = None

