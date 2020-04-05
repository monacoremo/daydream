'Tests for the `todos` API.'

import time

import requests


def test_create_todo(service_endpoint, alice_session):
    'A user should be able to create todo items.'

    current_user = alice_session.get(f'{service_endpoint}/api/rpc/current_user').json()[0]

    resp = alice_session.post(f'{service_endpoint}/api/todos', json={
        'user_id': current_user['user_id'],
        'description': 'Test todo',
    }, headers={'Prefer': 'return=representation'})

    assert resp.status_code == 201
    assert resp.json()[0]['description'] == 'Test todo'


def test_todo_visibility(service_endpoint, alice_session, bob_session):
    'A user should only be able to see his own and public todos.'

    current_user = alice_session.get(f'{service_endpoint}/api/rpc/current_user').json()[0]

    todo_desc = f'Alice test todo {time.time()}'

    resp = alice_session.post(f'{service_endpoint}/api/todos', json={
        'user_id': current_user['user_id'],
        'description': todo_desc,
    }, headers={'Prefer': 'return=representation'})
    assert resp.status_code == 201

    # Create a new todo item
    resp = alice_session.get(f'{service_endpoint}/api/todos')
    assert todo_desc in (todo['description'] for todo in resp.json())

    # The todo item should not be visible for other users
    resp = bob_session.get(f'{service_endpoint}/api/todos')
    assert todo_desc not in (todo['description'] for todo in resp.json())

    # Set all todo items from alice to be public
    resp = alice_session.patch(f'{service_endpoint}/api/todos', json={'public': True})
    assert resp.status_code == 204

    # Other users should now be able to see it
    resp = bob_session.get(f'{service_endpoint}/api/todos')
    assert todo_desc in (todo['description'] for todo in resp.json())


def test_create_todo_anonymous(service_endpoint, alice_session):
    'Anonymous users should not be able to create todo items.'

    resp = alice_session.post(f'{service_endpoint}/api/todos', json={
        'user_id': 1,
        'description': 'Test todo',
    })

    assert resp.status_code == 401
