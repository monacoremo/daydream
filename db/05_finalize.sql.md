# Finalize application setup

## Commit changes

We are done with defining the application and commit all changes:

```sql
commit;

```

## Run all tests

Run all tests in a transaction that will be rolled back:

```sql
\echo 'Running tests...'

begin;

select tests.run() "test results";

rollback;

\echo '...testing done.'

```

This will print out the test results to `stdout` and undo any changes that the
tests did to our data.

> ### Side-effect of running the tests
>
> The only visible trace from running the tests is going to be the state of the
> primary key sequences, e.g. `app.users_user_id_seq`, which will have a higher
> value than before. This is because PostgreSQL reserves new ids in a sequence
> for transactions in order to maintain high performance on concurrent inserts,
> and does not release them even if the transactions are rolled back. We could
> reset the sequences to their earlier value with something like `select
> setval('app.users_user_id_seq', max(user_id)) from app.users;` if we cared
> about that.

## Fixtures

Any fixtures could be added here with `insert` statements. If you use `copy`
statements or the `\copy` psql meta command, you'll need to reset the sequences.

Afterwards, we should `analyze` the current database in order to help the query
planner to be efficient.

```sql
analyze;

```
