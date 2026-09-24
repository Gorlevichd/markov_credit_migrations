"""Preparation of raw rating observations for modelling."""

from collections.abc import Iterable

import pandas as pd

from .ratings import Rating, normalize_rating


def prepare_ratings(
    frame: pd.DataFrame,
    *,
    agency: str = "S&P",
    rating_type: str = "LT Local Issuer Credit",
) -> pd.DataFrame:
    """Filter, normalize, and deduplicate raw rating records.

    The input follows the columns used by the source Bloomberg exports:
    ``Company Name``, ``Date``, ``Curr Rtg``, ``Agency``, and ``Rating Type``.
    The returned frame uses stable snake_case names and integer rating codes.
    """

    required = {"Company Name", "Date", "Curr Rtg", "Agency", "Rating Type"}
    missing = required.difference(frame.columns)
    if missing:
        raise ValueError(f"Missing required columns: {sorted(missing)}")

    result = frame.loc[
        (frame["Agency"] == agency) & (frame["Rating Type"] == rating_type)
    ].copy()
    result["date"] = pd.to_datetime(result["Date"], errors="coerce")
    result["rating"] = result["Curr Rtg"].map(normalize_rating)
    result = result.dropna(subset=["Company Name", "date", "rating"])
    result = result.rename(columns={"Company Name": "company_name"})
    result["rating"] = result["rating"].map(int)
    columns = ["company_name", "date", "rating"]
    if "Industry Type" in result:
        result = result.rename(columns={"Industry Type": "industry_type"})
        columns.append("industry_type")
    return (
        result[columns]
        .sort_values(["company_name", "date"])
        .drop_duplicates()
        .reset_index(drop=True)
    )


def stop_at_default(frame: pd.DataFrame) -> pd.DataFrame:
    """Keep each company's observations through its first default."""

    parts: list[pd.DataFrame] = []
    for _, company in frame.sort_values("date").groupby("company_name"):
        default_positions = company.index[company["rating"] == int(Rating.D)]
        if len(default_positions):
            company = company.loc[: default_positions[0]]
        parts.append(company)
    if not parts:
        return frame.copy()
    return pd.concat(parts).sort_values(["company_name", "date"]).reset_index(drop=True)

