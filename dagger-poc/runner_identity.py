"""Bind a reporting environment contract to actual measurement metadata."""

import hashlib
import json

from measurement import MEASUREMENT_PROFILE

REQUIRED_OBSERVATIONS = (
    "arch",
    "cpu_model",
    "kernel",
    "os_release",
    "cpu_max",
    "memory_max",
    "cpuset_cpus_effective",
)


def validate_identity(identity: dict) -> str:
    if not identity.get("runner_id") or identity.get("profile") != MEASUREMENT_PROFILE:
        raise ValueError("Runner identity must name the runner and current measurement profile")
    if not isinstance(identity.get("rounds"), int) or identity["rounds"] <= 0:
        raise ValueError("Runner identity requires a positive workload")
    if not identity.get("devbox_image"):
        raise ValueError("Runner identity requires its pinned container image")
    observed = identity.get("environment", {})
    if any(
        not isinstance(observed.get(key), str) or not observed[key] for key in REQUIRED_OBSERVATIONS
    ):
        raise ValueError("Runner identity lacks required observed hardware/OS/resource limits")
    return hashlib.sha256(
        json.dumps(identity, sort_keys=True, separators=(",", ":")).encode()
    ).hexdigest()


def bind_identity(result: dict, identity: dict, timeout: int | None) -> None:
    digest = validate_identity(identity)
    if (
        result.get("Rounds") != identity["rounds"]
        or result.get("DevboxImage") != identity["devbox_image"]
    ):
        raise ValueError("Measured workload or container image differs from runner identity")
    if result.get("MeasurementProfile") != identity["profile"]:
        raise ValueError("Measured protocol differs from runner identity")
    if identity.get("measurement_timeout_seconds") != timeout:
        raise ValueError("Measurement timeout differs from runner identity")
    actual = result.get("Environment", {})
    if any(actual.get(key) != value for key, value in identity["environment"].items()):
        raise ValueError("Observed hardware/OS/resource limits differ from runner identity")
    result["RunnerEnvironmentSHA256"] = digest
    result["RunnerID"] = identity["runner_id"]
