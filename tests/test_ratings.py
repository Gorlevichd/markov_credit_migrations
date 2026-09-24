from markov_credit_migrations.ratings import Rating, normalize_rating


def test_normalize_rating_collapses_agency_notches():
    assert normalize_rating("AA-") is Rating.AAA_A
    assert normalize_rating("BBB+") is Rating.BBB
    assert normalize_rating("Bpi") is Rating.BB_B
    assert normalize_rating("SD") is Rating.D


def test_unknown_rating_is_excluded():
    assert normalize_rating("ND") is None
    assert normalize_rating(None) is None

