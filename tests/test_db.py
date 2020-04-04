'''
Run database tests.

'''

from collections import namedtuple
import itertools
import os
import re
import subprocess
import time

import pytest
import tap.parser


DBTEST_BIN = os.environ.get('TESTS_DBTEST_BIN')


TapPlan = namedtuple('TapPlan', 'indent amount')
TapTestLine = namedtuple('TapTestLine', 'indent ok number description directive subtests')
TapSubtest = namedtuple('TapSubtest', 'indent name')


psql_separator_line = re.compile(r'-+')
psql_query_result_line = re.compile(r'\(\d+ rows\)')

tap_test_line = re.compile(r'(\s*)(ok|not ok) ((\d+) )?(.*)(#(.*))?')
tap_plan = re.compile(r'(\s*)1..(\d+)')
tap_subtest = re.compile(r'(\s*)# Subtest: (.*)')



def test_db():
    'Run database tests and parse TAP results.'

    completed = subprocess.run(DBTEST_BIN, check=True, capture_output=True)

    result_text = completed.stdout.decode('utf-8')
    result_lines = result_text.split('\n')

    assert result_lines[0].strip() == 'test results', 'unexpected result format'
    assert re.fullmatch(r'-+', result_lines[1]), 'unexpected result format'
    assert re.fullmatch(r'\(\d+ rows\)', result_lines[-3]), 'unexpected result format'

    tap_lines = [line[1:] for line in result_lines[2:-3]]

    parsed = [parse_tap_line(line) for line in tap_lines]

    assert all(line.ok for line in parsed if isinstance(line, TapTestLine)), 'all tests expected to pass'


def parse_tap_line(line):
    'Parse a line of TAP output'

    plan = tap_plan.match(line)
    test_line = tap_test_line.match(line)
    subtest = tap_subtest.match(line)

    if plan:
        indent, amount = plan.groups()
        return TapPlan(indent=indent, amount=int(amount))
    elif test_line:
        indent, ok, _, number, description, _, directive = test_line.groups()
        return TapTestLine(
            indent=indent,
            ok=ok == 'ok',
            number=int(number),
            description=description,
            directive=directive,
            subtests=None,
        )
    elif subtest:
        indent, name = subtest.groups()
        return TapSubtest(indent=indent, name=name)
