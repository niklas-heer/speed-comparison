# Prevent main() from running when loading scmeta for tests
ENV["SCMETA_TESTING"] = "1"

require "spec"
require "../src/scmeta"
