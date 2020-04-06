# Users

```sql
create table app.users (
    user_id bigserial primary key,
    email citext not null,
    name text not null,
    password text not null,
    unique (email)
);

comment on table app.users is 'Users of the application';

```

## Email validation

The unique constraint on email will make sure, that an email address can only
be used by one user at a time. PostgreSQL will create an index in order to
enforce this constraint, which will also make our login queries faster. The
`email` column is set to be case insensitive with the `citext` type, as we
don't want to allow the same email address to be used more than one time by
capitalizing it differently.

To validate the email, it would be best to create a [custom
domain](https://dba.stackexchange.com/a/165923):

create extension plperl;
create function validate_email(email citext)
returns boolean
language plperlu
immutable
leakproof
strict
as \$$
         use Email::Valid;
         my $email = shift;
Email::Valid->address($email) or die "Invalid email address: $email\n";
return 'true';
\$\$;

create domain valid_email as citext not null
constraint valid_email_check check (validate_email(value))

We could then use `valid_email` as the column type. We will skip this for this
example, as it would require another extension that might not be available by
default.

## Hashing passwords

We need to salt and hash all passwords, which we will ensure using a trigger.

```sql
create function app.cryptpassword ()
    returns trigger
    language plpgsql
    as $$
begin
    if tg_op = 'INSERT' or new.password <> old.password then
        new.password = crypt(new.password, gen_salt('bf'));
    end if;
    return new;
end
$$;

create trigger cryptpassword
    before insert or update on app.users for each row
    execute procedure app.cryptpassword ();

```

`app.cryptpassword` is a special kind of function that returns a trigger. We
use the PostgreSQL procedural language `plpgsql` to define it. We would prefer
to use plain SQL where possible to define functions, but using the procedural
language is necessary in this case. Trigger functions receive several implicit
arguments, including:

- `tg_op` will be set to the operator of the triggering query, e.g. `INSERT`,
  `UPDATE` etc.
- `old` will be set to the version of the record before the query was executed.
  For newly created created records with `INSERT`, there is no previous record
  and `old` will be set to null.
- `new` is the potential new record that resulted from the triggering query.

The trigger function returns a record that will be used instead of `new`.

The `begin` and `end` keywords have nothing to do with transactions here, they
are just special `plpgsql` syntax. There is also an optional `declare` section
that can be used before `begin` to declare variables, as we will see later.

The trigger we defined here will fire on any change of the `password` field and
make sure that only salted and hashed passwords are saved in the database.

## Permissions on the `users` table

The `auth` role will need to be able to reference the users and to select
certain fields in order to validate credentials:

```sql
grant references, select (user_id, email, password) on table app.users to auth;

```

We will grant selective permissions to our API:

```sql
grant select (user_id, name, email), insert (name, email, password), update
    (name, email, password) on table app.users to api;

```

The API will also need to work with the primary key sequence of `users` in order
to register new users:

```sql
grant all on app.users_user_id_seq to api;

```

> We could also grant those permissions in a separate section in order
> to completely decouple the API from this schema,
> but it seems more practicable to keep the permission grants
> close to the definition of each object.

## Row Level Security and policies

### Enable Row Level Security

We want to make sure that `users` and `todos` can only be accessed by who
is supposed to have access to them. As a first step, we are going to lock the
tables in the `app` schema down completely using Row Level Security:

```sql
alter table app.users enable row level security;

```

As of now, no user will be able to access any row in the `app` schema, with
the exception of the superuser. We will grant granular access to individual
roles using `policies`. As the superuser usually overrides Row Level Security,
we will need to make sure that no functions or views that access the `app`
schema are owned by the superuser.

PostgreSQL will make sure that our policies are consistently applied in all
cases, e.g. when performing joins of embeds. This would be very challenging to
implement reliably outside the database.

### Helper function: Current `user_id`

Our Row Level Security policies will need to access the `user_id` of the
currently authenticated user. See [`auth.authenticate`](#authentication-hook)
for the function that sets the value as a local setting.

```sql
create function app.current_user_id ()
    returns integer
    language sql
    as $$
    select
        nullif (current_setting('auth.user_id', true),
            '')::integer
$$;

comment on function app.current_user_id is 'User_id of the currently
                                              authenticated user, or null if not authenticated.';

```

We need to grant the roles that benefit from policies access to this function:

```sql
grant execute on function app.current_user_id to api, webuser;

```

### Policies on `app.users`

Web-users should be able to see all other users (we'll restrict the columns
through the API views), but only edit their own record.

```sql
create policy webuser_read_user on app.users for select using
    (current_setting('role') = 'webuser');

create policy webuser_update_user on app.users for update using
    (current_setting('role') = 'webuser'
    and user_id = app.current_user_id ());

```

Policies can be created for specific roles using a `to` clause, e.g. `create policy webuser_read_user to webuser for ...`. This would, however, not work for
this use-case. We will define views in a separate API schema that will be owned
by the `api` role. When a `webuser` uses those views, the policy checks would
be run against the role of the view owner, `api`. `current_setting('role')`
always refers to the current role that was set with `set local role ...;`
previously, so we use that instead.

The `auth` role will need to select users in order to validate their
credentials:

```sql
create policy auth_read_user on app.users for select to auth using (true);

```

Our API should be able to register new users:

```sql
create policy api_insert_user on app.users for insert to api with check (true);

```

## API

### Switch to `api` role

All following views and functions should be owned by the `api` role. The
easiest way to achieve this is to switch to it for now:

```sql
set role api;

```

If the views in the `api` schema were owned by the superuser, they would be
executed with the permissions of the superuser and bypass Row Level security.
We'll check with tests if we got it right in the end.

### Users API endpoint

We don't want our users to be able to access fields like `password` from
`app.users`. We can filter the columns in the view with which we expose that
table in our API.

```sql
create view api.users as
select
    user_id,
    name
from
    app.users;

```

We grant web-users selective permissions on that view:

```sql
grant select, update (name) on api.users to webuser;

```

Our Row Level Security policies will make sure that users will only be
able to update their own records.

Each user should be able to get more details on his own account. We will
restrict the user's access by defining a function for that purpose:

```sql
create type api.user as (
    user_id bigint,
    name text,
    email citext
);

create function api.current_user()
    returns api.user
    language sql
    security definer
    as $$
    select
        user_id,
        name,
        email
    from
        app.users
    where
        user_id = app.current_user_id ();

$$;

comment on function api.current_user is 'Information about the currently
                                              authenticated user';

grant execute on function api.current_user to webuser;

```

### Resetting role from `api` to the superuser

Now that the API is fully described in the `api` schema, we switch back to the
superuser role.

```sql
reset role;

```
