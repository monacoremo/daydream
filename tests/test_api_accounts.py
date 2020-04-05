'Tests for accounts and sessions.'

import time

import requests


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


def test_register_repeated(service_endpoint, alice_session):
    'Registering when already logged in should not be allowed.'

    resp = alice_session.post(f'{service_endpoint}/api/rpc/register', json={
        'email': f'registrationtest-{time.time()}@test.org',
        'name': 'Registration Test',
        'password': 'registrationsecret',
    })

    assert resp.status_code == 401
    assert resp.cookies.get('session_token') is None


def test_login(service_endpoint, alice_account, alice_email, alice_password):
    'Logging in with valid credentials should succeed.'

    session = requests.Session()
    resp = session.post(f'{service_endpoint}/api/rpc/login', json={
        'email': alice_email,
        'password': alice_password,
    })

    assert resp.status_code == 200
    assert resp.cookies.get('session_token')


def test_login_repeated(service_endpoint, alice_email, alice_session, alice_password):
    'A logged in user should not be able to log in again.'

    resp = alice_session.post(f'{service_endpoint}/api/rpc/login', json={
        'email': alice_email,
        'password': alice_password,
    })
    assert resp.status_code == 401


def test_login_wrong_email(service_endpoint, alice_account, alice_email, alice_password):
    'Logins with wrong emails should fail.'

    resp = requests.post(f'{service_endpoint}/api/rpc/login', json={
        'email': 'wrong_' + alice_email,
        'password': alice_password,
    })

    assert resp.status_code == 401


def test_login_wrong_password(service_endpoint, alice_account, alice_email, alice_password):
    'Logins with wrong passwords should fail.'

    resp = requests.post(f'{service_endpoint}/api/rpc/login', json={
        'email': alice_email,
        'password': 'wrong_' + alice_password,
    })

    assert resp.status_code == 401


def test_current_user(service_endpoint, alice_session):
    'Logged in users should be able to access information on their own account.'

    resp = requests.get(f'{service_endpoint}/api/rpc/current_user')
    assert resp.status_code == 401

    resp = alice_session.get(f'{service_endpoint}/api/rpc/current_user')
    assert resp.status_code == 200
    assert resp.json()[0]['name'] == 'Alice'


def test_logout(service_endpoint, alice_session):
    'Logging out should be possible for logged in users and change permissions.'

    resp = alice_session.get(f'{service_endpoint}/api/rpc/current_user')
    assert resp.status_code == 200

    resp = alice_session.post(f'{service_endpoint}/api/rpc/logout')
    assert resp.status_code == 200

    resp = alice_session.get(f'{service_endpoint}/api/rpc/current_user')
    assert resp.status_code == 401

    resp = alice_session.post(f'{service_endpoint}/api/rpc/logout')
    assert resp.status_code == 401


def test_refresh_session(service_endpoint, alice_session):
    'Refreshing a session should succeed and set a new cookie.'

    resp = alice_session.post(f'{service_endpoint}/api/rpc/refresh_session')

    assert resp.status_code == 200
    assert resp.cookies.get('session_token')


def test_users(service_endpoint, alice_session):
    'Logged in users should be able to get a listing of users.'

    current_user = alice_session.get(f'{service_endpoint}/api/rpc/current_user').json()[0]

    resp = alice_session.get(f'{service_endpoint}/api/users')

    assert resp.status_code == 200
    assert current_user['user_id'] in (user['user_id'] for user in resp.json())
