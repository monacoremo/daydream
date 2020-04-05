# Ordered trees

## Data definition

To model the hierarchical structure of the revenues in our example, we need
to keep track of the line item that each line item belongs to (the parent) and
the order of the line items that all have the same parent (siblings).

```sql
create table app.lineitems (
    lineitem_id serial primary key,
    parent_id int references app.lineitems (lineitem_id),
    position int not null,
    label text not null,
    constraint valid_lineitem_parent check (lineitem_id != parent_id),
    constraint valid_lineitem_position check (position >= 0),
    constraint unique_lineitem_position unique (parent_id, position) deferrable
	initially immediate
);

comment on table app.lineitems is 'Financial statements line items.';

comment on column app.lineitems.position is 'Position of the item among its siblings.';

```

Based on the contraints that we set up on our new table, only valid ordered
tree structures can be represented. It's impossible for a line item to have
a non-existing parent (`references`) and no line items can be in the same
position as one of its siblings.

Root nodes are reperesented by line items where the `parent_id` is `null`. An
alternative would be to disallow `null` values and to define root nodes as
ones that have themselves as a parent. However, it would be quite cumbersome
to create such nodes and we would need to support the process with a trigger.
So this seems like a better solution.

The `positions` column might have holes, e.g. a item might be at position
3 and the next one at 5, leaving a hole at position 4. This is not an issue
for us, as our data still describes a valid order of the siblings. It's
impossible to represent an invalide state.

```sql
alter table app.lineitems enable row level security;

```

## API

### Data queries

To work with our hierarchical line items, we will often need a listing of them
that includes the level and the number each item. In the example above, the
level is represented by the identation of each item. For example, if the item
'Insurance and Other' is at level 1, 'Leasing revenues' is at level 2; they are
in the first and third position among their siblings, respectively.

#### Line item type

```sql
set role api;

```

Let's create a new type to represent line items and their position in the
hierarchy:

```sql
create type api.lineitem as (
    lineitem_id int,
    parent_id int,
    number bigint,
    label text,
    level_ int,
    path_ integer[]
);

comment on type api.lineitem is 'Line item, including its level and path in the
                  tree of line items.';

```

The `path` will be used as an array of positions, which can be useful to
order the line items.

#### Line item subtrees

The subtrees of a parent line item are all the trees that descend from it. We will
use a recursive `with`-query (or Common Table Expression) to select all those
items for a given parent.

```sql
create function api.lineitem_subtrees (lineitem_id int)
    returns setof api.lineitem
    language sql
    as $$
    with recursive items as (
        select
            lineitem_id,
            parent_id,
            position,
            label,
            1 level_,
            array[]::integer[] path_
        from
            app.lineitems lineitems
        where
            lineitems.lineitem_id = lineitem_subtrees.lineitem_id
        union all
        select
            children.lineitem_id,
            children.parent_id,
            children.position,
            children.label,
            items.level_ + 1 level_,
            items.path_ || children.position path_
        from
            items,
            app.lineitems children
        where
            items.lineitem_id = children.parent_id
)
    select
        lineitem_id,
        parent_id,
        row_number() over (partition by parent_id order by position),
        label,
        level_,
        path_
    from
        items
    where
        items.lineitem_id != lineitem_subtrees.lineitem_id
    order by
        path_
$$;

comment on function api.lineitem_subtrees is 'Returns the tree of line items
                  starting from a root node.';

```

### Creating new line items

#### Inserting new line items as the first sibling after an existing line item

```sql
create function api.insert_lineitem_after (lineitem_id int, label text)
    returns int
    language plpgsql
    as $$
declare
    target app.lineitems;
    new_lineitem_id int;
begin
    -- find the item that the new item should be inserted after
    select
        *
    from
        app.lineitems lineitems
    where
        lineitems.lineitem_id = insert_lineitem_after.lineitem_id into strict target;
    -- make room for our new item
    set constraints unique_lineitem_position deferred;
    update
        app.lineitems
    set
        position = position + 1
    where
        parent_id = target.parent_id
        and position > target.position;
    set constraints unique_lineitem_position immediate;
    -- insert our new item
    insert into app.lineitems (parent_id, position, label)
        values (target.parent_id, target.position + 1, insert_lineitem_after.label)
    returning
        lineitems.lineitem_id into new_lineitem_id;
    return new_lineitem_id;
end;
$$;

comment on function api.insert_lineitem_after is 'Insert a new line item as the
                  first sibling after the given line item.';

```

#### Inserting new line items as the first child of a line item

```sql
create function api.insert_lineitem_first (parent_id int, label text)
    returns int
    language plpgsql
    as $$
declare
    new_lineitem_id int;
begin
    -- make room for the new item
    set constraints unique_lineitem_position deferred;
    update
        lineitems
    set
        position = position + 1
    where
        lineitems.parent_id = insert_lineitem_first.parent_id;
    set constraints unique_lineitem_position immediate;
    -- insert the new item
    insert into lineitems (parent_id, position, label)
        values (insert_lineitem_first.parent_id, 0, insert_lineitem_first.label)
    returning
        lineitems.lineitem_id into new_lineitem_id;
    return new_lineitem_id;
end;
$$;

comment on function api.insert_lineitem_first is 'Insert a new line item as the
                  first child of another item.';

```

### Deleting line items

#### Deleting a leaf line item and closing the hole it might have left behind

```sql
create function api.delete_lineitem (lineitem_id int)
    returns void
    language plpgsql
    as $$
declare
    target app.lineitems;
begin
    -- find the item to be deleted
    select
        *
    from
        app.lineitems lineitems
    where
        lineitems.lineitem_id = delete_lineitem.lineitem_id into strict target;
    -- delete the item
    delete from app.lineitems lineitems
    where lineitems.lineitem_id = delete_lineitem.lineitem_id;
    -- close the hole left by the deleted item
    set constraints unique_lineitem_position deferred;
    update
        app.lineitems
    set
        position = position - 1
    where
        parent_id = target.parent_id
        and position > target.position;
    set constraints unique_lineitem_position immediate;
end;
$$;

comment on function api.delete_lineitem is 'Delete the given line item.';

```

This function will not work on any items that have any children, as our
`references` constraint will prevent the deletion. We could specify
`on delete cascade` on it, but we would rather be explicit about such
destructive deletes and will define a separate function to implement it.

#### Deleting the subtrees of a line item

```sql
create function api.delete_lineitem_subtrees (lineitem_id int)
    returns void
    language sql
    as $$
    delete from app.lineitems
    where lineitem_id in (
            select
                lineitem_id
            from
                lineitem_subtrees (delete_lineitem_subtrees.lineitem_id))
$$;

comment on function api.delete_lineitem_subtrees is 'Delete the subtrees of the
                  given line item.';

```

#### Deleting a line item and its subtrees

```sql
create function api.delete_lineitem_including_subtrees (lineitem_id int)
    returns void
    language sql
    as $$
    select
        delete_lineitem_subtrees (lineitem_id);

select
    delete_lineitem (lineitem_id);

$$;

comment on function api.delete_lineitem_including_subtrees is 'Delete the
                  subtrees of the given line item and the given lineitem itself.';

```

### Moving line items

#### Moving line items to be the first sibling after an existing line item

To move a line item to any other location in our hierarchy, we need to perform
the following steps:

1. Make room for the line item at its intended new position, moving all the
   items with the same or higher position by one.
2. Move the line item there.
3. Close the hole in the `position`s it might have left behind.

```sql
create function api.move_lineitem_after (lineitem_id int, target_lineitem_id int)
    returns void
    language plpgsql
    as $$
declare
    item_to_move app.lineitems;
    target app.lineitems;
begin
    -- find the item that we want to move
    select
        *
    from
        app.lineitems lineitems
    where
        lineitems.lineitem_id = move_lineitem_after.lineitem_id into strict item_to_move;
    -- find the item that we want to move our item after
    select
        *
    from
        app.lineitems lineitems
    where
        lineitems.lineitem_id = target_lineitem_id into strict target;
    -- make room for the item to be moved
    set constraints unique_lineitem_position deferred;
    update
        app.lineitems
    set
        position = position + 1
    where
        parent_id = target.parent_id
        and position > target.position;
    set constraints unique_lineitem_position immediate;
    -- move the item
    set constraints unique_lineitem_position deferred;
    update
        app.lineitems lineitems
    set
        position = target.position + 1,
        parent_id = target.parent_id
    where
        lineitems.lineitem_id = move_lineitem_after.lineitem_id;
    set constraints unique_lineitem_position immediate;
    -- close the hole left by the moved item
    update
        app.lineitems
    set
        position = position - 1
    where
        parent_id = item_to_move.parent_id
        and position > item_to_move.position;
end;
$$;

comment on function api.move_lineitem_after is 'Move a line item to be the
                  first sibling after another.';

```

#### Moving a line item to be the first child of a parent line item

```sql
create function api.move_lineitem_first (lineitem_id int, parent_id int)
    returns void
    language plpgsql
    as $$
declare
    item_to_move app.lineitems;
begin
    -- find the item that we want to move
    select
        *
    from
        app.lineitems lineitems
    where
        lineitems.lineitem_id = move_lineitem_first.lineitem_id into strict item_to_move;
    -- make room for the item to be moved
    set constraints unique_lineitem_position deferred;
    update
        app.lineitems lineitems
    set
        position = position + 1
    where
        lineitems.parent_id = move_lineitem_first.parent_id;
    set constraints unique_lineitem_position immediate;
    -- move the item
    update
        app.lineitems lineitems
    set
        position = 0,
        parent_id = move_lineitem_first.parent_id
    where
        lineitems.lineitem_id = move_lineitem_first.lineitem_id;
    -- close the hole left by the moved item
    set constraints unique_lineitem_position deferred;
    update
        app.lineitems
    set
        position = position - 1
    where
        lineitems.parent_id = item_to_move.parent_id
        and position > item_to_move.position;
    set constraints unique_lineitem_position immediate;
end;
$$;

comment on function api.move_lineitem_first is 'Move a line item to be the
                  first child of another item.';

```
