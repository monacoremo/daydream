# Todos

In this example application we will manage to-do items, as they are simple and
still well suited to demonstrate the security mechanisms and PostgREST
features. Let's say that, in order to show the permissions and Row Level
Security mechanisms, we want to make the items visible to their owner and, if
they are set as public, to anyone.

```sql
create table app.todos (
    todo_id bigserial primary key,
    user_id bigint references app.users,
    description text not null,
    created timestamptz not null default clock_timestamp(),
    done bool not null default false,
    public bool not null default false,
    unique (user_id, description)
);

comment on table app.todos is 'Todo items that can optionally be set to public.';

comment on column app.todos.public is 'Todo item will be visible to all users
                    if public.';

```

The unique constraint will make sure, that each user can only have one todo item
with a specific title.

Our API will get access to the `app.todo` table:

```sql
grant select, insert (user_id, description, public), update (description, done,
    public), delete on table app.todos to api;

```

Web users will also need access to the sequence of the `todos` primary key,
so that they can insert new rows:

```sql
grant all on app.todos_todo_id_seq to webuser;

```

This seems to work without actually granting the role `usage` on this schema.

> #### Note: Granting permissions
>
> A pragmatic way to figure out which permissions need to be granted is to
> start with a locked down setup (as we will do here with separated schemas and
> roles, Row Level Security and revoked default permissions), write tests for
> your application (be it unit tests in the database, see below, or integration
> tests) and running them while adding permissions step by step until
> everything works. You don't need to come up with those queries from nothing!

### Row Level Security and policies

#### Enable Row Level Security

We want to make sure that `users` and `todos` can only be accessed by who
is supposed to have access to them. As a first step, we are going to lock the
tables in the `app` schema down completely using Row Level Security:

```sql
alter table app.todos enable row level security;

```

As of now, no user will be able to access any row in the `app` schema, with
the exception of the superuser. We will grant granular access to individual
roles using `policies`. As the superuser usually overrides Row Level Security,
we will need to make sure that no functions or views that access the `app`
schema are owned by the superuser.

PostgreSQL will make sure that our policies are consistently applied in all
cases, e.g. when performing joins of embeds. This would be very challenging to
implement reliably outside the database.

### Access to `app.todos`

Users should be able to read todo items that they own or that are public.
They should only be able to write their own todo items.

```sql
create policy webuser_read_todo on app.todos for select using
    (current_setting('role') = 'webuser'
    and (public or user_id = app.current_user_id ()));

create policy webuser_write_todo on app.todos for all using
    (current_setting('role') = 'webuser'
    and user_id = app.current_user_id ());

```

## API

```sql
set role api;

```

### Todos API endpoint

We will expose the todo items through a view:

```sql
create view api.todos as
select
    todo_id,
    user_id,
    description,
    public,
    created,
    done
from
    app.todos;

comment on view api.todos is 'Todo items that can optionally be set to be public.';

```

Web-users should be able to view, create, update and delete todo items, with the
restrictions that we previously set in the Row Level Security policies.

```sql
grant select, insert, update (description, public, done), delete on api.todos
    to webuser;

```

### Resetting role from `api` to the superuser

Now that the API is fully described in the `api` schema, we switch back to the
superuser role.

```sql
reset role;

```
