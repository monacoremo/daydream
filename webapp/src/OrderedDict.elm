module OrderedDict exposing
    ( OrderedDict
    , dropIf
    , empty
    , eq
    , foldl
    , foldr
    , fromList
    , get
    , insert
    , isEmpty
    , keepIf
    , keys
    , map
    , memberOf
    , remove
    , singleton
    , size
    , toList
    , update
    , values
    )

import List
import Sort exposing (Sorter)
import Sort.Dict as Dict exposing (Dict)


type OrderedDict k v
    = OrderedDict (List k) (Dict k v)


empty : Sorter k -> OrderedDict k v
empty sorter =
    OrderedDict [] (Dict.empty sorter)


singleton : Sorter k -> k -> v -> OrderedDict k v
singleton sorter key value =
    insert key value (empty sorter)


insert : k -> v -> OrderedDict k v -> OrderedDict k v
insert key value odict =
    update key (always (Just value)) odict


remove : k -> OrderedDict k v -> OrderedDict k v
remove key odict =
    update key (always Nothing) odict


update : k -> (Maybe v -> Maybe v) -> OrderedDict k v -> OrderedDict k v
update key updater (OrderedDict list dict) =
    OrderedDict
        (case updater (Dict.get key dict) of
            Just _ ->
                if List.member key list then
                    list

                else
                    list ++ [ key ]

            Nothing ->
                if List.member key list then
                    List.filter (\k -> k /= key) list

                else
                    list
        )
        (Dict.update key updater dict)



-- QUERY


isEmpty : OrderedDict k v -> Bool
isEmpty odict =
    size odict == 0


size : OrderedDict k v -> Int
size (OrderedDict list _) =
    List.length list


get : k -> OrderedDict k v -> Maybe v
get key (OrderedDict _ dict) =
    Dict.get key dict


memberOf : OrderedDict k v -> k -> Bool
memberOf (OrderedDict _ dict) key =
    Dict.memberOf dict key


eq : OrderedDict k v -> OrderedDict k v -> Bool
eq (OrderedDict listA dictA) (OrderedDict listB dictB) =
    (listA == listB) && Dict.eq dictA dictB



-- TRANSFORM


map : (k -> a -> b) -> OrderedDict k a -> OrderedDict k b
map func (OrderedDict list dict) =
    OrderedDict list (Dict.map func dict)


keepIf : (k -> v -> Bool) -> OrderedDict k v -> OrderedDict k v
keepIf keep (OrderedDict list dict) =
    let
        newDict =
            Dict.keepIf keep dict
    in
    OrderedDict
        (List.filter (Dict.memberOf newDict) list)
        newDict


dropIf : (k -> v -> Bool) -> OrderedDict k v -> OrderedDict k v
dropIf drop odict =
    keepIf (\key value -> not (drop key value)) odict


foldl : (k -> v -> b -> b) -> b -> OrderedDict k v -> b
foldl f acc (OrderedDict list dict) =
    List.foldl (reducer dict f) acc list


foldr : (k -> v -> b -> b) -> b -> OrderedDict k v -> b
foldr f acc (OrderedDict list dict) =
    List.foldr (reducer dict f) acc list


reducer : Dict k v -> (k -> v -> b -> b) -> (k -> b -> b)
reducer dict f key acc =
    case Dict.get key dict of
        Just value ->
            f key value acc

        Nothing ->
            acc



-- LISTS


keys : OrderedDict k v -> List k
keys (OrderedDict list _) =
    list


values : OrderedDict k v -> List v
values (OrderedDict list dict) =
    List.filterMap (\key -> Dict.get key dict) list


toList : OrderedDict k v -> List ( k, v )
toList odict =
    List.map2 Tuple.pair (keys odict) (values odict)


fromList : Sorter k -> List ( k, v ) -> OrderedDict k v
fromList sorter assocs =
    OrderedDict
        (List.map Tuple.first assocs)
        (Dict.fromList sorter assocs)
