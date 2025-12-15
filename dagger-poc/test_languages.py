"""
Tests for language configuration validation.

Run with: pytest test_languages.py -v
"""

from pathlib import Path

import pytest

from languages import (
    LANGUAGES,
    Language,
    get_all_versions,
    get_languages_by_category,
    get_variants,
)

# Path to source files (relative to repo root, not dagger-poc)
SRC_DIR = Path(__file__).parent.parent / "src"


class TestLanguageDefinitions:
    """Test that all language definitions are valid."""

    def test_all_source_files_exist(self):
        """Every language must have its source file in src/."""
        missing = []
        for target, lang in LANGUAGES.items():
            src_path = SRC_DIR / lang.file
            if not src_path.exists():
                missing.append(f"{target}: {lang.file}")

        if missing:
            pytest.fail(f"Missing source files:\n" + "\n".join(missing))

    def test_file_names_valid(self):
        """All source files must follow valid naming convention."""
        for target, lang in LANGUAGES.items():
            filename = lang.file.split("/")[-1].lower()
            # Allow leibniz.*, leibniz-simd.*, Leibniz.*, or subdir/file patterns
            valid = filename.startswith("leibniz") or "/" in lang.file
            assert valid, f"{target}: file must start with leibniz or be in subdir, got {lang.file}"

    def test_names_are_not_empty(self):
        """Language names must not be empty."""
        for target, lang in LANGUAGES.items():
            assert lang.name, f"{target}: name is empty"

    def test_run_commands_are_not_empty(self):
        """Run commands must not be empty."""
        for target, lang in LANGUAGES.items():
            assert lang.run, f"{target}: run command is empty"

    def test_nixpkgs_have_versions(self):
        """All nixpkgs must have explicit versions."""
        for target, lang in LANGUAGES.items():
            for pkg in lang.nixpkgs:
                assert "@" in pkg, (
                    f"{target}: package '{pkg}' must have explicit version (e.g., '{pkg}@1.0.0')"
                )

    def test_no_duplicate_names(self):
        """Display names should be unique (for chart clarity)."""
        names = [lang.name for lang in LANGUAGES.values()]
        duplicates = [n for n in names if names.count(n) > 1]
        if duplicates:
            pytest.fail(f"Duplicate display names: {set(duplicates)}")

    def test_compiled_languages_have_compile_command(self):
        """Languages with binary output should have compile commands."""
        # These are known interpreted languages that don't need compilation
        interpreted = {"python", "pypy", "ruby", "nodejs", "bun", "lua", "luajit", "perl", "php"}

        for target, lang in LANGUAGES.items():
            if target not in interpreted and lang.run.startswith("./"):
                assert lang.compile, (
                    f"{target}: runs binary ({lang.run}) but has no compile command"
                )

    def test_all_languages_have_base(self):
        """All languages should have a base defined for icon mapping."""
        for target, lang in LANGUAGES.items():
            assert lang.base, f"{target}: missing 'base' for icon mapping"


class TestLanguageDataclass:
    """Test Language dataclass validation."""

    def test_invalid_file_raises(self):
        """Creating Language with invalid file should raise."""
        with pytest.raises(ValueError, match="must start with leibniz"):
            Language(
                name="Test",
                nixpkgs=["test@1.0.0"],
                file="invalid.c",  # Should be leibniz.c
                run="./test",
            )

    def test_empty_name_raises(self):
        """Creating Language with empty name should raise."""
        with pytest.raises(ValueError, match="name is required"):
            Language(
                name="",
                nixpkgs=["test@1.0.0"],
                file="leibniz.c",
                run="./test",
            )

    def test_missing_version_raises(self):
        """Creating Language without version in nixpkgs should raise."""
        with pytest.raises(ValueError, match="must have version"):
            Language(
                name="Test",
                nixpkgs=("test",),  # Missing @version (tuple format)
                file="leibniz.c",
                run="./test",
            )

    def test_primary_package_extracts_name(self):
        """primary_package should extract package name without version."""
        lang = Language(
            name="Test",
            nixpkgs=("rustc@1.91.1",),
            file="leibniz.rs",
            run="./test",
        )
        assert lang.primary_package == "rustc"

    def test_primary_version_extracts_version(self):
        """primary_version should extract version from package."""
        lang = Language(
            name="Test",
            nixpkgs=("rustc@1.91.1",),
            file="leibniz.rs",
            run="./test",
        )
        assert lang.primary_version == "1.91.1"

    def test_icon_key_uses_base(self):
        """icon_key should use base if set."""
        lang = Language(
            name="C (clang)",
            nixpkgs=("clang@18.0.0",),
            file="leibniz.c",
            run="./test",
            base="c",
        )
        assert lang.icon_key == "c"

    def test_extract_version_with_regex(self):
        """extract_version should use custom regex."""
        lang = Language(
            name="Go",
            nixpkgs=("go@1.23.0",),
            file="leibniz.go",
            run="./test",
            version_regex=r"go(\d+\.\d+\.?\d*)",
        )
        assert lang.extract_version("go version go1.23.4 linux/amd64") == "1.23.4"

    def test_missing_packages_raises(self):
        """Language without any packages should raise."""
        with pytest.raises(ValueError, match="must have nixpkgs"):
            Language(
                name="Test",
                file="leibniz.c",
                run="./test",
            )


class TestCategoryGrouping:
    """Test category utilities."""

    def test_get_languages_by_category(self):
        """Should group languages correctly."""
        categories = get_languages_by_category()

        # Should have some known categories
        assert "systems" in categories
        assert "interpreted" in categories

        # Each language should appear exactly once
        all_langs = []
        for langs in categories.values():
            all_langs.extend(langs)

        assert len(all_langs) == len(LANGUAGES)
        assert set(all_langs) == set(LANGUAGES.keys())


class TestVariants:
    """Test variant utilities."""

    def test_get_variants_python(self):
        """Should find Python variants."""
        variants = get_variants("python")
        assert "python" in variants
        assert "pypy" in variants

    def test_get_variants_c(self):
        """Should find C variants."""
        variants = get_variants("c")
        assert "c" in variants
        assert "c-clang" in variants


class TestVersionUtilities:
    """Test version extraction utilities."""

    def test_get_all_versions(self):
        """get_all_versions should return all pinned versions."""
        versions = get_all_versions()

        assert isinstance(versions, dict)
        assert len(versions) == len(LANGUAGES)

        # All values should be version strings
        for target, version in versions.items():
            assert version, f"{target}: version is empty"
            # Version should not contain @ (that's the package format)
            assert "@" not in version, f"{target}: version contains @"
