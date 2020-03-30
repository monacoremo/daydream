# Basic setup

## psql script settings

To begin, we set the script to be quiet and to stop if an error occurs:

```sql
\set QUIET on
\set ON_ERROR_STOP on

```

This uses the `\set` [Meta
Command](https://www.postgresql.org/docs/12/app-psql.html#APP-PSQL-META-COMMANDS)
of `psql` to set the respective
[variables](https://www.postgresql.org/docs/12/app-psql.html#APP-PSQL-VARIABLES).
We will use more `psql` Meta Commands in the course of this script.

### Create extensions

In this application, we are going to use the `pgcrypto` extension to salt and
hash passwords and to generate random session tokens. `citext` will provide us
with a case-insensitive text type which we will use for emails. `pgTAP` allows
us to define and run tests within the database. While the first two extensions
come with PostgreSQL by default, you will need to install the `pgTAP` one
separately.

```sql
\echo 'Creating extensions...'

create extension pgcrypto;
create extension citext;
create extension pgtap;

```

## Roles

Roles in PostgreSQL apply to the database cluster (i.e. the set of databases
managed by one PostgreSQL server) as a whole. Setting up the roles for our
application is the only part that might encounter conflicts when run on a fresh
database. We are going to set up all the required roles here.

### Authenticator role and its sub-roles

PostgREST will log in as the `authenticator` role and switch to either the
`anonymous` or `webuser` roles, based on the results of authentication.

```sql
\echo 'Setting up roles...'

create role authenticator noinherit login;

comment on role authenticator is
    'Role that serves as an entry-point for API servers such as PostgREST.';

create role anonymous nologin noinherit;

comment on role anonymous is
    'The role that PostgREST will switch to when a user is not authenticated.';

create role webuser nologin noinherit;

comment on role webuser is
    'Role that PostgREST will switch to for authenticated web users.';

```

As we won't use the JWT authentication feature, PostgREST will always switch
to the `anonymous` role first. We will then switch roles again if the user
is authenticated based on the session logic defined in
[`auth.authenticate`](#authentication-hook).

The comments will be useful to users working with our schema, both in GUI
applications and with `psql` meta commands, e.g. `\d...`.

We need to allow the authenticator role to switch to the other roles:

```sql
grant anonymous, webuser to authenticator;

```

When deploying this application, you can set a password for the authenticator
user by running `alter role authenticator set password to '...';`.
Alternatively, you can use the `psql` meta command `\password authenticator`
interactively, which will make sure that the password does not appear In any
logs or history files.

### Auth and API roles

The `auth` and `api` roles will own their respective schemas including the
tables, views and functions defined in them.

```sql
create role auth nologin;

comment on role auth is
    'Role that owns the auth schema and its objects.';

create role api nologin;

comment on role api is
    'Role that owns the api schema and its objects.';

```

You might choose to add more roles and even separate APIs with fine grained
privileges when your application grows.

### Revoke default execute privileges on newly defined functions

By default, all database users (identified by the role `PUBLIC`, which is
granted to all roles by default) have privileges to execute any function that
we define. To be safe, we are going to change this default:

```sql
alter default privileges revoke execute on functions from public;

```

Now, for all functions created in this database by the superuser, permissions
to execute functions have to be explicitly granted using `grant execute on
function ...` statements.

We also need to remove the default execute privileges from the `auth` and `api`
roles, as the defaults apply per user.

```sql
alter default privileges for role auth, api revoke execute on functions from public;

```

## App schema

The `app` schema will contain the current state and business logic of the
application. We will define the authentication functionalities and our API in
separate schemas later and isolate all PostgREST specific parts there.

```sql
\echo 'Creating the app schema...'

create schema app;

comment on schema app is
    'Schema that contains the state and business logic of the application.';

```

In this example, the `app` schema will be owned by the superuser (usually
`postgres`). In larger application, it might make sense to have it owned by a
separate role with lower privileges.


### Usage permissions on the `app` schema

The views owned by the api schema will be executed with its permissions,
regardless of who is using the views. Accordingly, we grant the api role
access to the data schema, but restrict access through the row level
security policies.

```sql
grant usage on schema app to auth, api;

```

## API schema

The `api` schema defines an API on top of our application that will be exposed
to PostgREST. We could define several different APIs or maintain an API even
though the underlying application changes.

```sql
\echo 'Creating the api schema...'

create schema authorization api;

comment on schema api is
    'Schema that defines an API suitable to be exposed through PostgREST';

```

By using the `authorization` keyword, the newly created `api` schema will be
owned by the `api` role.


### Grant users access to the `api` schema

The user roles need the `usage` permission on the `api` schema before they can
do anything with it:

```sql
grant usage on schema api to anonymous, webuser;

```


## Tests

We need to make sure that the permissions and policies that we set up actually
work. The following tests will be maintained with the database schema and can
be run whenever needed, e.g. after migrations.

```sql
\echo 'Setting up tests...'

begin;

create schema tests;

```


### Test our schema setup

We will use [pgTAP](https://pgtap.org/) functions to describe our tests. You'll
find a full listing of the assertions functions you can user in the [pgTAP
documentation](https://pgtap.org/documentation.html).

```sql
create function tests.test_schemas()
    returns setof text
    language plpgsql
    as $$
    begin
        return next schemas_are(ARRAY[
            'app',
            'auth',
            'api',
            'tests',
            'public'
        ]);

        return next tables_are(
            'app',
            ARRAY[
                'users',
                'todos'
            ]
        );

        return next ok(
            (select bool_and(rowsecurity = true)
                from pg_tables
                where schemaname = 'app'
            ),
            'Row level security should be enabled for all tables in schema app'
        );

        return next tables_are('api', array[]::name[]);

        return next view_owner_is('api', 'users', 'api'::name);
        return next view_owner_is('api', 'todos', 'api'::name);

        return next schema_privs_are('app', 'api', array['USAGE']);
        return next schema_privs_are('tests', 'api', array[]::name[]);

        -- anonymous and webuser roles should have no direct access to the app schema.
        return next schema_privs_are('app', 'webuser', array[]::name[]);
        return next schema_privs_are('app', 'anonymous', array[]::name[]);
    end;
    $$;

comment on function tests.test_schemas is
    'Test that the schemas and the access to them is set up correctly.';

```

### Test role memberships

The `authenticator` role needs to be granted the `anonymous` and `webuser`
roles.

```sql
create function tests.test_roles()
    returns setof text
    language plpgsql
    as $$
    begin
        return next is_member_of('anonymous', ARRAY['authenticator']);
        return next is_member_of('webuser', ARRAY['authenticator']);
    end;
    $$;

comment on function tests.test_roles is
    'Make sure that the roles are set up correctly.';

```


### Test runner

To conclude the `tests` schema, we set up a function that we can call anytime to
run all tests.

```sql
create function tests.run()
    returns setof text
    language sql
    as $$
        select runtests('tests'::name, '^test_');
    $$;

```
