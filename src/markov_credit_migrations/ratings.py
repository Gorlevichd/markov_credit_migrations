"""Credit-rating states and normalization helpers."""

from enum import IntEnum
import re


class Rating(IntEnum):
    """Ordered rating states used by the model."""

    AAA_A = 0
    BBB = 1
    BB_B = 2
    CCC_C = 3
    D = 4


RATING_LABELS = {
    Rating.AAA_A: "AAA-A",
    Rating.BBB: "BBB",
    Rating.BB_B: "BB-B",
    Rating.CCC_C: "CCC-C",
    Rating.D: "D",
}


def normalize_rating(value: object) -> Rating | None:
    """Map a raw Bloomberg/S&P rating to the model's five states.

    Unknown, empty, and ``ND`` values return ``None`` so callers can remove
    them explicitly instead of silently assigning an incorrect state.
    """

    if value is None:
        return None
    raw = str(value).strip().upper()
    if not raw or raw == "ND":
        return None

    raw = re.sub(r"[+*\-]", "", raw)
    raw = raw.replace("PI", "").replace("T", "").replace("U", "")
    raw = raw.replace("SD", "D").replace("R", "D").replace("P", "")
    raw = raw.replace("()", "")

    if raw in {"AAA", "AA", "A"}:
        return Rating.AAA_A
    if raw == "BBB":
        return Rating.BBB
    if raw in {"BB", "B"}:
        return Rating.BB_B
    if raw in {"CCC", "CC", "C"}:
        return Rating.CCC_C
    if raw == "D":
        return Rating.D
    return None

