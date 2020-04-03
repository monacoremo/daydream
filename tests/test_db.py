'''
Run database tests.

'''

import os
import subprocess
import time

import pytest


DB_URI = os.environ.get('TESTS_DB_URI')


def test_pgtap():
    pass
