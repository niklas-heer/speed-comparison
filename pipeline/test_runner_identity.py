import pytest

from measurement import MEASUREMENT_PROFILE
from runner_identity import REQUIRED_OBSERVATIONS, bind_identity


def test_only_matching_actual_environment_gets_checkpoint_identity():
    expected = {
        "runner_id": "test-worker",
        "profile": MEASUREMENT_PROFILE,
        "rounds": 10000,
        "devbox_image": "pinned-image",
        "environment": {key: "recorded-test-value" for key in REQUIRED_OBSERVATIONS},
    }
    raw = {
        "Rounds": 10000,
        "DevboxImage": "pinned-image",
        "MeasurementProfile": MEASUREMENT_PROFILE,
        "Environment": dict(expected["environment"]),
    }
    bind_identity(raw, expected, None)
    assert len(raw["RunnerEnvironmentSHA256"]) == 64
    for changed in [{"Rounds": 100}, {"Environment": {**raw["Environment"], "cpu_max": "changed"}}]:
        candidate = {**raw, **changed}
        candidate.pop("RunnerEnvironmentSHA256")
        with pytest.raises(ValueError):
            bind_identity(candidate, expected, None)
        assert "RunnerEnvironmentSHA256" not in candidate
