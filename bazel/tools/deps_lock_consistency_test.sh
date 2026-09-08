#!/usr/bin/env bash
# Every Maven coordinate deps.edn declares must appear in deps.lock.json.
#
# The lockfile is resolved offline of the build by rules_clj's //tools/lock and
# committed; nothing in a build re-resolves it, so adding a dependency and
# forgetting to relock produces a build that simply cannot see it. This is the
# cheap half of that check — a coordinate present here does not prove the
# lockfile's transitive closure is current, but a coordinate MISSING proves it
# is not, and that is the mistake anyone actually makes.
set -euo pipefail

DEPS_EDN="${1:?path to deps.edn}"
LOCK="${2:?path to deps.lock.json}"

status=0
while read -r group_artifact version; do
  [ -n "$group_artifact" ] || continue
  if ! grep -qF "\"coordinates\": \"${group_artifact}:${version}\"" "$LOCK"; then
    echo "FAIL: deps.edn declares ${group_artifact} ${version}, absent from $(basename "$LOCK")" >&2
    status=1
  fi
done < <(grep -oE '[A-Za-z0-9._-]+/[A-Za-z0-9._-]+[[:space:]]*\{:mvn/version[[:space:]]*"[^"]+"' "$DEPS_EDN" |
         sed -E 's/[[:space:]]*\{:mvn\/version[[:space:]]*"/ /; s/"$//')

if [ "$status" -eq 0 ]; then
  echo "ok: every deps.edn coordinate is in the lockfile"
fi
exit "$status"
