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


## Test schema

We need to make sure that the permissions and policies that we set up actually
work. The following tests will be maintained with the database schema and can
be run whenever needed, e.g. after migrations.

```sql
\echo 'Setting up tests...'

create schema tests;

```


## Tests

### Test schemas

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
