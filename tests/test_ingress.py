"""
Tests for the ingress endpoint.

"""

import requests


def test_index(service_endpoint):
    "Index should be available."
    resp = requests.get(f"{service_endpoint}/")
    assert resp.status_code == 200


def test_appjs(service_endpoint):
    "App.js should be available"
    resp = requests.get(f"{service_endpoint}/app.js")
    assert resp.status_code == 200


def test_docs(service_endpoint):
    "Documentation should be available"
    resp = requests.get(f"{service_endpoint}/docs/")
    assert resp.status_code == 200
    assert "Welcome" in resp.text
