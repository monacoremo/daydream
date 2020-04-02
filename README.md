# Daydream

> Work in progress

## Overview

This is an experiment on developing full stack application in a completely
different way.

* The core of the application is defined directly in
  [Postgres](https://www.postgresql.org/) using SQL, leveraging the power of
  Row Level Security and other features that the database offers
* [PostgREST](http://postgrest.org/) is used to automatically derive a HTTP API
  from the Postgres application
* All changes to the application state are tracked though an events service
  which is implemented in [Haskell](https://www.haskell.org/), leveraging the
  power of sum types to keep everything consistent
* All APIs are tied together using [Openresty](https://openresty.org/) /
  [Nginx](https://www.nginx.com/) as a reverse HTTP proyx and Lua for
  middleware tasks
* The frontend is written in [Elm](https://elm-lang.org/), with all API bindings
  generated from the Postgres application and events service
* All dependencies and build steps are managed with [Nix](https://nixos.org/)
* Tests are set up for all levels of the stack, including integration tests
  written in Python and with Selenium

## Running the application

It's possible to run a local instance of the application with one command, e.g.:

```
nix-shell --run daydream-local-watch
```

This takes care of everything from getting dependencies, setting up the
database, spinning up and tying together the services and building the frontend
with generated bindings. It offers a view on all logs, a bit like
`docker-compose` and includes hot code reload for all modules.

## No containers needed

The command show above is able to run completely locally without containers. All
dependencies are managed by Nix and all working files are set up in a temporary
directory, so there are no permanent changes to your machine. At the same time,
containers for deployment are automatically built from the same source.

## Why?

This experiment might yield a radically more efficient and maintainable way to
write full stack applications. The combination of SQL on Postgres, PostgREST,
Haskell, Openresty/Nginx, Lua, Elm, Nix and Python, while optionally running
everything locally without containers might need getting used to. But it has
many upsides:

* **Less work and repetition:** Almost all the APIs and frontend bindings are
  generated from the database schema
* **High reliability:** All dependencies kept consistent with Nix. The integrity
  of the application state is maintained directly at the core in the database,
  avoiding whole classes of bugs
* ...more fun! :-)
