'''
Integration tests for the PostgREST sessions full stack example.

'''

import os
import subprocess
import time

import requests
import pytest


BASE_URL = os.environ.get('TESTS_BASE_URI')


def test_index():
    'Index should be available.'
    resp = requests.get(f'{BASE_URL}/')
    assert resp.status_code == 200


def test_appjs():
    'App.js should be available'
    resp = requests.get(f'{BASE_URL}/app.js')
    assert resp.status_code == 200


def test_register():
    'Registering as an anonymous user should succeed.'
    session = requests.Session()
    resp = session.post(f'{BASE_URL}/api/rpc/register', json={
        'email': f'registrationtest-{time.time()}@test.org',
        'name': 'Registration Test',
        'password': 'registrationsecret',
    })

    assert resp.status_code == 200
    assert resp.cookies.get('session_token')
