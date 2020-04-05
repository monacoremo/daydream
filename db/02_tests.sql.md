## Tests

Our tests will be maintained with the database schema and can be run whenever
needed, e.g. after migrations. All tests will be registered in the tests schema:

```sql
\echo 'Setting up tests...'
create schema tests;

```

We can use the following function to run all tests that have been registered in
this schema.

```sql
create function tests.run ()
    returns setof text
    language sql
    as $$
    select
        runtests ('tests'::name,
            '^test_');

$$;

```
