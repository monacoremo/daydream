# Extensions

In this application, we are going to use the `pgcrypto` extension to salt and
hash passwords and to generate random session tokens. `citext` will provide us
with a case-insensitive text type which we will use for emails. `pgTAP` allows
us to define and run tests within the database.

```sql
\echo 'Creating extensions...'
create extension pgcrypto;

create extension citext;

create extension pgtap;

```
