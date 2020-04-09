"""
Run web app tests.

"""

import os
import subprocess


WEBAPP_TEST_BIN = os.environ.get("TESTS_WEBAPP_BIN")


def test_webapp():
    "Run webapp tests."

    subprocess.run(WEBAPP_TEST_BIN, check=True)
