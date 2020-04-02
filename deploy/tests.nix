{ settings, python, checkedShellScript, deployLocal, curl, entr }:

let
  testPython =
    python.withPackages
      (
        ps: [
          ps.pytest
          ps.requests
        ]
      );

  binPrefix =
    "${settings.binPrefix}tests-";
in
rec {
  run =
    checkedShellScript "${binPrefix}run"
      ''
        set -e

        tmpdir="$(mktemp -d)"
        fullstack-local-mkenv . "$tmpdir"
        # shellcheck source=/dev/null
        source "$tmpdir/env"

        cleanup() {
            rm -rf "$tmpdir"
            kill 0
        }

        trap cleanup exit

        ${deployLocal.run} &

        printf "Waiting for app to become ready."

        checkurl="${settings.URI}"/healthcheck

        status() {
            ${curl}/bin/curl -s -o /dev/null -w '%{http_code}' "$checkurl"
        }

        while [[ "$(status)" != "200" ]]; do
            printf "."
            sleep 0.1;
        done

        echo " done."
        echo "Running tests..."

        ${python}/bin/py.test tests/tests.py "$@"
      '';

  watch =
    checkedShellScript "${binPrefix}watch"
      ''
        find "${settings.sourceDir}/.." | ${entr}/bin/entr -r ${run}
      '';
}
