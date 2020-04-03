'''
Integration tests for the PostgREST sessions full stack example.

'''
import time

import requests


def test_index(service_endpoint):
    'Index should be available.'
    resp = requests.get(f'{service_endpoint}/')
    assert resp.status_code == 200


def test_appjs(service_endpoint):
    'App.js should be available'
    resp = requests.get(f'{service_endpoint}/app.js')
    assert resp.status_code == 200


def test_register(service_endpoint):
    'Registering as an anonymous user should succeed.'
    session = requests.Session()
    resp = session.post(f'{service_endpoint}/api/rpc/register', json={
        'email': f'registrationtest-{time.time()}@test.org',
        'name': 'Registration Test',
        'password': 'registrationsecret',
    })

    assert resp.status_code == 200
    assert resp.cookies.get('session_token')
