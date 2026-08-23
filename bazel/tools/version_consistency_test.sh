#!/usr/bin/env bash
# version.edn is the single version source; the README install snippet must
# agree with it. build.clj and the release workflow read version.edn directly,
# so this snippet is the only copy a machine does not otherwise check.
set -euo pipefail

VERSION_EDN="${1:?path to version.edn}"
README="${2:?path to README.md}"

declared=$(grep -oE '\{:version "[^"]+"' "$VERSION_EDN" | cut -d'"' -f2)
[ -n "$declared" ] || {
  echo "FAIL: no version declared in $VERSION_EDN" >&2
  exit 1
}
echo "version.edn declares $declared"

readme=$(grep -oE 'clj-protobuf \{:mvn/version "[^"]+"' "$README" | head -1 | cut -d'"' -f2)
if [ -z "$readme" ]; then
  echo "FAIL: no :mvn/version install snippet found in README" >&2
  exit 1
elif [ "$readme" != "$declared" ]; then
  echo "FAIL: README pins '$readme' but version.edn declares '$declared'" >&2
  exit 1
fi
echo "ok: README agrees ($readme)"
