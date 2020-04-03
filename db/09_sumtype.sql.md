# Sum type

Experiment on how to represent sum types in Postgres.

In this example, we try to represent the following sum type:

    type Animal
        = Dog { name : String, age : Integer }
        | Bird { song : String }

```sql
create type app.animal_type
    as enum
        ( 'dog'
        , 'bird'
        );


create table app.animals
    ( animal_id serial primary key
    , animal_type app.animal_type not null
    , dog_name text
        check ((animal_type = 'dog') = (dog_name is not null))
    , dog_age integer
        check ((animal_type = 'dog') = (dog_age is not null))
    , bird_song text
        check ((animal_type = 'bird') = (bird_song is not null))
    );

```

```sql
alter table app.animals enable row level security;

```

## Fixtures

Create some fixtures:

```sql
insert into app.animals(animal_type, bird_song) values ('bird', 'lalala');
insert into app.animals(animal_type, dog_name, dog_age) values ('dog', 'rex', 7);

```

The following examples would fail the check constraints:

Value from another constructor:

    insert into animals(animal_type, bird_song, dog_name)
        values ('bird', 'lalala', 'rex');

Not all fields set for the constructor:

    insert into animals(animal_type, dog_name) values ('dog', 'rex');

## API

```sql
set role api;

create view api.animals as
    select * from app.animals;

reset role;

```
