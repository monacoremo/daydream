### Switch to the `auth` role

All following tables and functions should be owned by the `auth` role. The
easiest way to achieve this is to switch to it for now:

```sql
set role auth;

```

We will be able to return to the superuser role later with `reset role;`.

### Sessions

We will use a table to track user sessions:

```sql
create table auth.sessions (
    token text not null primary key default encode(gen_random_bytes(32), 'base64'),
    user_id integer not null references app.users,
    created timestamptz not null default clock_timestamp(),
    expires timestamptz not null default clock_timestamp() + '15min' ::interval,
    check (expires > created)
);

comment on table auth.sessions is 'User sessions, both active and expired ones.';

comment on column auth.sessions.expires is 'Time on which the session expires.';

```

The `token` field will be generated automatically based on 32 random bytes
(i.e. 256 bit, which should be plenty) from the `pgcrypto` module, which will
then be base64 encoded. We could also store the raw bytes in a `bytea` column,
saving a bit of space, and handle the encoding and decoding in the API. But
the solution we chose here is much simpler and good enough for now.

`expires` will be set to the time 15 minutes into the future by default. You
can change this default with `alter column app.sessions.expires set default to clock_timestamp() + '...'::interval;`. The function `clock_timestamp()` will
always return the current time, independent from when the current transaction
started (other than, for example, `now()`).

We use a check constraint here to have the database maintain some invariant on
our data, in this case that a session should not expire before it was created.
With good constraints, we can prevent whole classes of bugs in our application.

In most places in our application, only the sessions that are currently active
will be of interest. We will create a view that identifies them reliably and
that we will be able to build upon later.

```sql
create view auth.active_sessions as
select
    token,
    user_id,
    created,
    expires
from
    auth.sessions
where
    expires > clock_timestamp(
)
    with local check option;

comment on view auth.active_sessions is 'View of the currently active sessions';

```

The `with local check option` statement enables checks on changes that operate
on this view, making sure that only valid sessions can be inserted or updated.

Filtering on the `expires` column, as we do in the view, would currently require
a very inefficient scan of the whole table on each query. We can make this more
efficient with an index.

```sql
create index on auth.sessions (expires);

```

To clean up expired sessions, we can periodically run the following function:

```sql
create function auth.clean_sessions ()
    returns void
    language sql
    security definer
    as $$
    delete from auth.sessions
    where expires < clock_timestamp() - '1day'::interval;

$$;

comment on function auth.clean_sessions is 'Cleans up sessions that have
                                     expired longer than a day ago.';

```

To run this function regularly, we could create a separate role with limited
privileges, granting it just `usage` on the `auth` schema and `execute` on this
function, that the cron job will be able to login as.

### Login

We define a login function that creates a new session, using many of the
defaults that we set in the `sessions` table.

```sql
create function auth.login (email text, password text)
    returns text
    language sql
    security definer
    as $$
    insert into auth.active_sessions (user_id)
    select
        user_id
    from
        app.users
    where
        email = login.email
        and password = crypt(login.password, password)
    returning
        token;

$$;

comment on function auth.login is 'Returns the token for a newly created
                                     session or null on failure.';

```

There is a lot happening here, so let's go through it step by step:

- The login function takes two parameters of type `text`, email and password,
  and returns a scalar value of type `text`, which will be a newly generated
  session token.
- `language sql` means that the function body will be a regular SQL query. We
  try to use SQL queries where possible, as they can be optimized the most by
  the query planner of PostgreSQL. If we are not able to express a function in
  regular SQL, we'll use the more complex and flexible `plpgsql` procedural
  language.
- `security definer` means that the function will run the permissions of the
  owner of the function (i.e. `auth` is this case), and not the permissions of
  the caller. This can create security risks if misused, but also gives us the
  opportunity to isolate and manage privileged actions used properly.
- `$$` is an alternative syntax for starting and ending strings, with the
  advantage that almost nothing needs to be escaped within this kind of string.
- The function body will create a new session if it finds a `user_id` that
  matches the given credentials. It creates a new session token and expiration
  time based on the defaults that we in the table and returns the new session
  token.

  Inserting into the `auth.active_sessions` view is possible, as it is simple
  enough for PostgreSQL to transparently translate it into an insert into
  `auth.sessions` (see:
  [Updatable views](https://www.postgresql.org/docs/12/sql-createview.html#SQL-CREATEVIEW-UPDATABLE-VIEWS)).

- The arguments given to the function can be accessed by the names given in the
  function definition. In order to disambiguate them from the columns of the
  `app.active_sessions` view, we can prefix them with the name of the function,
  `login` in this case (without the schema).
- The returned token is generated automatically based on the default value
  defined for the `token` column in the `app.sessions` table. If no new session
  has been created, i.e. because the credentials were not valid, then `null` will
  be returned by the function.

Anonymous users will need to be able to use this function. Our API role will
also need to use it, in order to log a user in directly after registration.

```sql
grant execute on function auth.login to anonymous, api;

```

### Refresh session

To refresh session, we update the expiration time in the respective record:

```sql
create function auth.refresh_session (session_token text)
    returns void
    language sql
    security definer
    as $$
    update
        auth.sessions
    set
        expires = default
    where
        token = session_token
        and expires > clock_timestamp()
$$;

comment on function auth.refresh_session is 'Extend the expiration time of the
                                     given session.';

```

We cannot use the `auth.active_sessions` view here, as the column default on
expires from the table `auth.sessions` is not available in the view.

Only authenticated user need to use this function:

```sql
grant execute on function auth.refresh_session to webuser;

```

### Logout

We expire sessions by setting their expiration time to the current time:

```sql
create function auth.logout (token text)
    returns void
    language sql
    security definer
    as $$
    update
        auth.sessions
    set
        expires = clock_timestamp()
    where
        token = logout.token
$$;

comment on function auth.logout is 'Expire the given session.';

grant execute on function auth.logout to webuser;

```

### Session User-ID

In our authentication hook `auth.authenticate`, we will need to get the
`user_id` of the currently authenticated user given a session token. We will
expose this privileged functionality through a `security definer` function that
will run with the permissions of the superuser.

```sql
create function auth.session_user_id (session_token text)
    returns integer
    language sql
    security definer
    as $$
    select
        user_id
    from
        auth.active_sessions
    where
        token = session_token;

$$;

comment on function auth.session_user_id is 'Returns the id of the user
                                     currently authenticated, given a session token';

```

The anonymous role will need to access this function in order to authenticate
itself:

```sql
grant execute on function auth.session_user_id to anonymous;

```

The query in this function will be efficient based on the primary key
index on the `token` column.

### Authentication hook

For each request, PostgREST will provide cookie values from the original HTTP
request it received in the `request.cookie.*` variables. In the authentication
hook that we define below, we will read the `session_token` cookie, if it
exists. The function will switch roles and set the appropriate `user_id` if the
session as identified by the token is valid.

```sql
create function auth.authenticate ()
    returns void
    language plpgsql
    as $$
declare
    session_token text;
    session_user_id int;
begin
    select
        current_setting('request.cookie.session_token', true) into session_token;
    select
        auth.session_user_id (session_token) into session_user_id;
    if session_user_id is not null then
        set local role to webuser;
        perform
            set_config('auth.user_id', session_user_id::text, true);
    else
        set local role to anonymous;
        perform
            set_config('auth.user_id', '', true);
    end if;
end;
$$;

comment on function auth.authenticate is 'Sets the role and user_id based on
                                     the session token given as a cookie.';

grant execute on function auth.authenticate to anonymous;

```

We need to take care to use `set local ...` statements or the function
`set_config(..., ..., true)` in order to absolutely make sure that we don't leak
settings between requests. Those variants set variables that are valid
only for the current transaction and PostgREST runs each request in its own
transaction.

> #### Note on developing functions
>
> As with permissions, it usually makes sense to develop functions step by step
> and to iterate on them using tests. For 'print statement debugging' in
> `plpgsql` functions, you can use statements like `raise warning 'Test: %', var;`, where `var` is a variable that will be formatted into the string at
> `%`.

We will configure PostgREST to run this function before every request in
[`postgrest.conf`](postgrest.conf) using `pre-request = "auth.authenticate"`.

### Usage permission on the `auth` schema

The `api`, `anonymous` and `webuser` roles will need to work with this schema:

```sql
grant usage on schema auth to api, anonymous, webuser;

```

### Resetting role from `auth` to the superuser

We are done with setting up our `auth` schema, so we switch back to the
superuser.

```sql
reset role;

```

# API

```sql
set role api;

```

### Login API endpoint

The `api.login` endpoint wraps the `auth.login` function to add the following:

- Raise an exception if the given login credentials are not valid.
- Add a header to the response to set a cookie with the session token.

```sql
create function api.login (email text, password text)
    returns void
    language plpgsql
    as $$
declare
    session_token text;
begin
    select
        auth.login (email,
            password) into session_token;
    if session_token is null then
        raise insufficient_privilege
        using detail = 'invalid credentials';
    end if;
    perform
	set_config('response.headers', '[{"Set-Cookie": "session_token=' ||
	    session_token || '; Path=/; Max-Age=600; HttpOnly"}]', true);
end;
$$;

comment on function api.login is 'Creates a new session given valid credentials.';

grant execute on function api.login to anonymous;

```

The `response.headers` setting will be read by PostgREST as a JSON list of
headers when the transaction completes, which it will then set as headers in
its HTTP response.

For this example, we set the cookie to expire after 600s or 10 minutes. This is
a conservative value that is shorter than the session duration according to our
business logic. Our frontend clients should refresh the session regularly as
long as the user is active.

### Refresh session API endpoint

In addition to the `refresh_session` function in `auth`, the
`api.refresh_session` variant will also update the lifetime of the session
cookie.

```sql
create function api.refresh_session ()
    returns void
    language plpgsql
    as $$
declare
    session_token text;
begin
    select
        current_setting('request.cookie.session_token', false) into strict session_token;
    perform
        auth.refresh_session (session_token);
    perform
	set_config('response.headers', '[{"Set-Cookie": "session_token=' ||
	    session_token || '; Path=/; Max-Age=600; HttpOnly"}]', true);
end;
$$;

comment on function api.refresh_session is 'Reset the expiration time of the
                                     given session.';

grant execute on function api.refresh_session to webuser;

```

See the [login endpoint](#login-api-endpoint) regarding the cookie
lifetime.

### Logout API endpoint

`api.logout` will expire the session using `auth.logout` and unset the session
cookie.

```sql
create function api.logout ()
    returns void
    language plpgsql
    as $$
begin
    perform
        auth.logout (current_setting('request.cookie.session_token', true));
    perform
        set_config('response.headers', '[{"Set-Cookie": "session_token=; Path=/"}]', true);
end;
$$;

comment on function api.logout is 'Expires the given session and resets the
                                     session cookie.';

grant execute on function api.logout to webuser;

```

### Register API endpoint

The registration endpoint will register a new user and create a new session.

```sql
create function api.register (email text, name text, password text)
    returns void
    security definer
    language plpgsql
    as $$
begin
    insert into app.users (email, name, password)
        values (register.email, register.name, register.password);
    perform
        api.login (email,
            password);
end;
$$;

comment on function api.register is 'Registers a new user and creates a new
                                     session for that account.';

```

Only unauthenticated users should be able to register:

```sql
grant execute on function api.register to anonymous;

```

### Resetting role from `api` to the superuser

Now that the API is fully described in the `api` schema, we switch back to the
superuser role.

```sql
reset role;

```

## Tests

### Helper function for impersonating users in tests

We will need to repeatedly impersonate users for our tests, so let's define a
helper function to help us with that:

```sql
create function tests.impersonate (role name, user_id integer)
    returns text
    language plpgsql
    as $$
begin
    select
        set_config('app.user_id', userid::text, true);
    set role to role;
end;
$$;

comment on function tests.impersonate is 'Impersonate the given role and user.';

```

### Test authorization functions

Tests for the authorization functions:

```sql
create function tests.test_auth ()
    returns setof text
    language plpgsql
    as $tests$
declare
    alice_user_id bigint;
    bob_user_id bigint;
    session_token text;
    session_expires timestamptz;
    session_expires_refreshed timestamptz;
    user_info record;
begin
    insert into app.users (email, name, password)
        values ('alice@test.org', 'Alice', 'alicesecret')
    returning
        user_id into alice_user_id;
    insert into app.users (email, name, password)
        values ('bob@test.org', 'Bob', 'bobsecret')
    returning
        user_id into alice_user_id;
    -- invalid password
    select
        auth.login ('alice@test.org',
            'invalid') into session_token;
    return next is (session_token,
        null,
        'No session should be created with an invalid password');
    -- invalid email
    select
        auth.login ('invalid',
            'alicesecret') into session_token;
    return next is (session_token,
        null,
        'No session should be created with an invalid user');
    -- valid login returns session token
    select
        auth.login ('alice@test.org',
            'alicesecret') into session_token;
    return next isnt (session_token,
        null,
        'Session token should be created for valid credentials');
    -- invalid login via the api
    prepare invalid_api_login as
    select
        api.login ('bob@test.org',
            'invalid');
    return next throws_ok ('invalid_api_login',
        'insufficient_privilege',
        'The api.login endpoint should throw on invalid logins');
    -- login via the api
    select
        api.login ('bob@test.org',
            'bobsecret') into user_info;
    return next isnt (user_info,
        null,
        'The api.login endpoint should return the user data');
    reset role;
    -- remember the current expiry time for later tests
    select
        sessions.expires into session_expires
    from
        auth.active_sessions sessions
    where
        token = session_token;
    -- check for valid session
    return next ok (exists (
            select
                1
            from
                auth.active_sessions
            where
                user_id = alice_user_id), 'There should be a session for the logged in user');
    perform
        set_config('request.cookie.session_token', session_token, true);
    set role webuser;
    perform
        api.refresh_session ();
    reset role;
    select
        sessions.expires into session_expires_refreshed
    from
        auth.active_sessions sessions
    where
        token = session_token;
    return next ok (session_expires < session_expires_refreshed,
        'Sessions should expire later when refreshed.');
    -- logging out
    set role webuser;
    perform
        api.logout ();
    reset role;
    return next ok (not exists (
            select
                1
            from
                auth.active_sessions
            where
                token = session_token), 'There should be no active session after logging out');
end;
$tests$;

```
