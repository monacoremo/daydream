# Roles

Roles in PostgreSQL apply to the database cluster (i.e. the set of databases
managed by one PostgreSQL server) as a whole. Setting up the roles for our
application is the only part that might encounter conflicts when run on a fresh
database. We are going to set up all the required roles here.

## Authenticator role and its sub-roles

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

## Auth and API roles

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

## Revoke default execute privileges on newly defined functions

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

## Test role memberships

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
