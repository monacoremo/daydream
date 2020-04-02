module Postgrest exposing
    ( Schema, schema
    , Attribute, string, int, float, bool, nullable
    , attribute
    , Relationship, HasOne, hasOne, HasMany, hasMany, HasNullable, hasNullable
    , Request
    , readAll, readOne, readMany
    , Selection
    , field, succeed
    , map, map2, map3, map4, map5, map6, map7, map8
    , embedAll, embedOne, embedNullable, embedMany
    , Condition
    , like, ilike, eq, gte, gt, lte, lt, is
    , true, false
    , not, all, any
    , Order, asc, desc
    , Direction(..), Nulls(..), order
    , createOne, createMany, updateOne, updateMany, deleteOne, deleteMany
    , Changeset, change, batch
    --, toHttpRequest
    --, readPage
    )

{-| Make PostgREST requests in Elm.

Copied from <https://github.com/john-kelly/elm-postgrest> in order to experiment
with it/figure out contributions.


### 1. Define the Schema

  - [Schema](#define-your-schema)
  - [Attributes](#attributes)
  - [Relationships](#relationships)


### 2. Build the Request

  - [Read Requests](#build-a-request)
  - [Selecting](#selecting)
  - [Filtering](#filtering)
  - [Ordering](#ordering)
  - [Write Requests](#write-requests)


### 3. Send the Request

  - [HTTP](#toHttpRequest)


# Define your Schema

Before you can do anything interesting (ex. fetching resources), you must first define your schema. The schema is a description of the available resources for your (Postg)REST API. The schema answers questions like: "What resources are available to me?", "What attributes can I select?" and "What relationships can I embed?". Once defined, all requests are built _from_ this schema. One cool benefit of this design choice is that if your schema correctly represents your REST API's data model, this package ensures that you are (for the most part!) incapable of constructing invalid requests.

@docs Schema, schema


## Attributes

A Schema is made up of Attributes. An `Attribute` represents a single field of an exposed resource.

@docs Attribute, string, int, float, bool, nullable


### Custom Attributes

@docs attribute


## Relationships

In addition to Attributes, a Schema is made up of Relationships. The way Relationships are modeled is one of the big ideas of this package.

@docs Relationship, HasOne, hasOne, HasMany, hasMany, HasNullable, hasNullable


# Build a Request

PostgREST supports all of the CRUD (Create, Read, Update, Delete) operations that you know and love! Let's start with the reads.

@docs Request


## Read Requests

@docs readAll, readOne, readMany, readPage


## Selecting

Describe what fields and embeds you'd like to select. The API is pretty similar to JSON Decoders.

@docs Selection


### Basic Selections

@docs field, succeed


### Mapping Selections

@docs map, map2, map3, map4, map5, map6, map7, map8


### Embed Selections

@docs embedAll, embedOne, embedNullable, embedMany


## Filtering

Describe how you'd like your resources filtered.

@docs Condition


### Basic Conditions

@docs like, ilike, eq, gte, gt, lte, lt, is


### Boolean Conditions

@docs true, false


### Combining Conditions

@docs not, all, any


## Ordering

Describe how you'd like your resources ordered.

@docs Order, asc, desc


### Advanced Ordering

@docs Direction, Nulls, order


## Write Requests

In addition to reading, we can perform the CRUD write operations.


### Create, Update, Delete

@docs createOne, createMany, updateOne, updateMany, deleteOne, deleteMany
@docs Changeset, change, batch


# Send your Request

@docs toHttpRequest

-}

import Dict exposing (Dict)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Url.Builder as Builder exposing (QueryParameter)


{-| You'll define a `Schema` for each available resource of your PostgREST server. There are 2 type parameters to a `Schema`:

  - `id`: type tag which uniquely identifies _this_ `Schema`

> The `id` type parameter is optional, however, **strongly encouraged**. If not specified, the Elm compiler is unable to let you know when you're trying to make an invalid embed.

> Additionally, you should note that the type of `id` can be _anything_, however, in practice the type will always be an empty custom type which _you define_. More on this later.

  - `attributes`: record of the available `Attribute` to select and `Relationship` to embed

> Once again, you should note that the type of `attributes` can be _anything_, however, in practice the type will always be a record of `Attribute` and `Relationship`.

-}
type Schema id attributes
    = Schema String attributes


{-| -}
type Attribute a
    = Attribute
        { name : String
        , decoder : Decode.Decoder a
        , encoder : a -> Encode.Value
        , toString : a -> String
        }



-- https://wiki.haskell.org/Empty_type
-- https://wiki.haskell.org/Phantom_type


{-| Represents an embed path between 2 `Schema`. There are 2 type parameters to a `Relationship`:

  - `cardinality`: type tag which represents the _kind_ of `Relationship` you're modeling

> The `cardinality` is automatically determinded by the functions which return a `Relationship` (ie. hasMany, hasOne, hasNullable), so don't worry about constructing values for this type!

  - `id`: type tag which uniquely identifies the _other_ `Schema` that this `Schema` is related to

> Just like the `id` of a `Schema`, the `id` of a `Relationship` is optional, however, **strongly encouraged**. If not specified, the Elm compiler is unable to let you know when you're trying to make an invalid embed.

-}
type Relationship cardinality id
    = Relationship String


{-| Model a "has many" relationship
-}
type HasMany
    = HasMany HasMany


{-| Model a "has one" relationship
-}
type HasOne
    = HasOne HasOne


{-| Model a "has one nullable" relationship
-}
type HasNullable
    = HasNullable HasNullable


{-| -}
type Request a
    = Read
        { parameters : Parameters
        , decoder : Decode.Decoder a
        }
    | Update
        { parameters : Parameters
        , decoder : Decode.Decoder a
        , value : Encode.Value
        }
    | Create
        { parameters : Parameters
        , decoder : Decode.Decoder a
        , value : Encode.Value
        }
    | Delete
        { parameters : Parameters
        , decoder : Decode.Decoder a
        }
    | Page
        { parameters : Parameters
        , expect : Http.Expect a
        }


type Parameters
    = Parameters
        { schemaName : String
        , attributeNames : List String
        , cardinality : Cardinality
        }
        (List Embed)


type alias Embed =
    ( String, Parameters )


type Cardinality
    = One (Result Bool Condition_)
    | Many
        { order : Maybe Orders
        , where_ : Result Bool Condition_
        , limit : Maybe Int
        , offset : Maybe Int
        }


{-| -}
type Selection attributes a
    = Selection
        (attributes
         ->
            { attributeNames : List String
            , embeds : List Embed
            , decoder : Decode.Decoder a
            }
        )


{-| In order to build our create/update requests, we need a way to _describe_ the data changes we want. That's where the Changeset comes in.
-}
type Changeset attributes
    = Changeset (attributes -> List ( String, Encode.Value ))


{-| -}
type Condition attributes
    = CBoolean Bool
    | COperator
        { negated : Bool
        , operator : Operator
        , getKeyValue : attributes -> { key : String, value : String }
        }
    | CCombine
        { negated : Bool
        , operator : BinaryLogicalOperator
        , conditions : List (Condition attributes)
        }


type Condition_
    = COperator_
        { negated : Bool
        , operator : Operator
        , key : String
        , value : String
        }
    | CCombine_
        { negated : Bool
        , operator : BinaryLogicalOperator
        , conditions : List Condition_
        }



{-
   Abbreviation     Meaning                      Postgres Equivalent

   eq                 equals                          =
   gt                 greater than                 >
   gte                 greater than or equal          >=
   lt                 less than                      <
   lte                 less than or equal              <=
   neq                 not equal                      <> or !=

   like             LIKE operator                LIKE
                    (use * in place of %)

   ilike             ILIKE operator               ILIKE
                    (use * in place of %)

   in                 one of a list of values      IN
                    e.g. ?a=in.1,2,3

   is                 checking for exact           IS
                    equality (null,true,false)

   fts                 full-text search using       @@
                    to_tsquery

   cs                 contains                     @>
                    e.g. ?tags=cs.{example, new}

   cd                 contained in                 <@
                    e.g. ?values=cd.{1,2,3}

   ov                 overlap                      &&
                    e.g. ?period=ov.[2017-01-01,2017-06-30]

   sl                 strictly left of,            <<
                    e.g. ?range=sl.(1,10)

   sr                 strictly right of              >>

   nxr                 does not extend to the       &<
                    right of
                    e.g. ?range=nxr.(1,10)

   nxl                 does not extend to the       &>
                    left of

   adj                 is adjacent to,              -|-
                    e.g. ?range=adj.(1,10)

-}


type Operator
    = Eq
    | Gt
    | Gte
    | Lt
    | Lte
    | Neq
    | Like
    | Ilike
    | In
    | Is
    | Fts
    | Cs
    | Cd
    | Ov
    | Sl
    | Sr
    | Nxr
    | Nxl
    | Adj


type BinaryLogicalOperator
    = And
    | Or


{-| -}
type Order attributes
    = Order
        { direction : Direction
        , nulls : Nulls
        , getKey : attributes -> String
        }


{-| -}
type Direction
    = Asc
    | Desc


{-| -}
type Nulls
    = NullsFirst
    | NullsLast


type Order_
    = Order_
        { direction : Direction
        , nulls : Nulls
        , key : String
        }


type alias Orders =
    ( Order_, List Order_ )


{-| Let's take a look at the definition of `Schema.school` originally referenced in the example found in the README of this package.

    -- Schema.elm
    type School
        = School School

    school :
        Schema School
            { id : Attribute Int
            , name : Attribute String
            , state : Attribute String
            }
    school =
        schema "schools"
            { id = string "unitid"
            , name = string "instnm"
            , state = string "stabbr"
            }

If you think about it, a `Schema` is nothing more than an opaque wrapper around _any_ 2 types. This is important to note because it means that it's incredibly easy to define your `Schema` incorrectly! Luckily, once your `Schema` is correct, your requests will pretty much always be valid.

-}
schema : String -> attributes -> Schema id attributes
schema name attrs =
    Schema name attrs


{-| -}
attribute :
    { decoder : Decode.Decoder a
    , encoder : a -> Encode.Value
    , toString : a -> String
    }
    -> String
    -> Attribute a
attribute { decoder, encoder, toString } name =
    Attribute
        { name = name
        , decoder = decoder
        , encoder = encoder
        , toString = toString
        }


{-| -}
int : String -> Attribute Int
int name =
    Attribute
        { name = name
        , decoder = Decode.int
        , encoder = Encode.int
        , toString = String.fromInt
        }


{-| -}
string : String -> Attribute String
string name =
    Attribute
        { name = name
        , decoder = Decode.string
        , encoder = Encode.string
        , toString = identity
        }


{-| -}
float : String -> Attribute Float
float name =
    Attribute
        { name = name
        , decoder = Decode.float
        , encoder = Encode.float
        , toString = String.fromFloat
        }


{-| -}
bool : String -> Attribute Bool
bool name =
    Attribute
        { name = name
        , decoder = Decode.bool
        , encoder = Encode.bool
        , toString =
            \b ->
                if b then
                    "true"

                else
                    "false"
        }


{-| -}
nullable : Attribute a -> Attribute (Maybe a)
nullable (Attribute { name, decoder, encoder, toString }) =
    let
        newToString maybeVal =
            case maybeVal of
                Just val ->
                    toString val

                Nothing ->
                    "null"

        newEncoder maybeVal =
            case maybeVal of
                Just val ->
                    encoder val

                Nothing ->
                    Encode.null
    in
    Attribute
        { name = name
        , decoder = Decode.nullable decoder
        , encoder = newEncoder
        , toString = newToString
        }


{-| **Important Note:** `String` parameter is the name of the foreign key
-}
hasOne : String -> Relationship HasOne id
hasOne =
    Relationship


{-| **Important Note:** `String` parameter depends on data model:

  - Many to Many: name of through table
  - One to Many: name of foreign key

-}
hasMany : String -> Relationship HasMany id
hasMany =
    Relationship


{-| **Important Note:** `String` parameter is the name of the foreign key
-}
hasNullable : String -> Relationship HasNullable id
hasNullable =
    Relationship


{-| -}
succeed : a -> Selection attributes a
succeed a =
    Selection <|
        \_ ->
            { attributeNames = []
            , embeds = []
            , decoder = Decode.succeed a
            }


{-| -}
field : (attributes -> Attribute a) -> Selection attributes a
field getter =
    Selection <|
        \attributes ->
            let
                (Attribute { decoder, name }) =
                    getter attributes
            in
            { attributeNames = [ name ]
            , embeds = []
            , decoder = Decode.field name decoder
            }


{-| -}
map : (a -> b) -> Selection attributes a -> Selection attributes b
map fn (Selection getSelection) =
    Selection <|
        \attributes ->
            let
                selection =
                    getSelection attributes
            in
            { attributeNames = selection.attributeNames
            , embeds = selection.embeds
            , decoder = Decode.map fn selection.decoder
            }


{-| -}
map2 : (a -> b -> c) -> Selection attributes a -> Selection attributes b -> Selection attributes c
map2 fn (Selection getSelectionA) (Selection getSelectionB) =
    Selection <|
        \attributes ->
            let
                selectionA =
                    getSelectionA attributes

                selectionB =
                    getSelectionB attributes
            in
            { attributeNames = selectionA.attributeNames ++ selectionB.attributeNames
            , embeds = selectionA.embeds ++ selectionB.embeds
            , decoder = Decode.map2 fn selectionA.decoder selectionB.decoder
            }


andMap : Selection attributes a -> Selection attributes (a -> b) -> Selection attributes b
andMap =
    map2 (|>)


{-| -}
map3 :
    (a -> b -> c -> d)
    -> Selection attributes a
    -> Selection attributes b
    -> Selection attributes c
    -> Selection attributes d
map3 fn selectionA selectionB selectionC =
    map fn selectionA
        |> andMap selectionB
        |> andMap selectionC


{-| -}
map4 :
    (a -> b -> c -> d -> e)
    -> Selection attributes a
    -> Selection attributes b
    -> Selection attributes c
    -> Selection attributes d
    -> Selection attributes e
map4 fn selectionA selectionB selectionC selectionD =
    map fn selectionA
        |> andMap selectionB
        |> andMap selectionC
        |> andMap selectionD


{-| -}
map5 :
    (a -> b -> c -> d -> e -> f)
    -> Selection attributes a
    -> Selection attributes b
    -> Selection attributes c
    -> Selection attributes d
    -> Selection attributes e
    -> Selection attributes f
map5 fn selectionA selectionB selectionC selectionD selectionE =
    map fn selectionA
        |> andMap selectionB
        |> andMap selectionC
        |> andMap selectionD
        |> andMap selectionE


{-| -}
map6 :
    (a -> b -> c -> d -> e -> f -> g)
    -> Selection attributes a
    -> Selection attributes b
    -> Selection attributes c
    -> Selection attributes d
    -> Selection attributes e
    -> Selection attributes f
    -> Selection attributes g
map6 fn selectionA selectionB selectionC selectionD selectionE selectionF =
    map fn selectionA
        |> andMap selectionB
        |> andMap selectionC
        |> andMap selectionD
        |> andMap selectionE
        |> andMap selectionF


{-| -}
map7 :
    (a -> b -> c -> d -> e -> f -> g -> h)
    -> Selection attributes a
    -> Selection attributes b
    -> Selection attributes c
    -> Selection attributes d
    -> Selection attributes e
    -> Selection attributes f
    -> Selection attributes g
    -> Selection attributes h
map7 fn selectionA selectionB selectionC selectionD selectionE selectionF selectionG =
    map fn selectionA
        |> andMap selectionB
        |> andMap selectionC
        |> andMap selectionD
        |> andMap selectionE
        |> andMap selectionF
        |> andMap selectionG


{-| -}
map8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i)
    -> Selection attributes a
    -> Selection attributes b
    -> Selection attributes c
    -> Selection attributes d
    -> Selection attributes e
    -> Selection attributes f
    -> Selection attributes g
    -> Selection attributes h
    -> Selection attributes i
map8 fn selectionA selectionB selectionC selectionD selectionE selectionF selectionG selectionH =
    map fn selectionA
        |> andMap selectionB
        |> andMap selectionC
        |> andMap selectionD
        |> andMap selectionE
        |> andMap selectionF
        |> andMap selectionG
        |> andMap selectionH


{-| -}
embedOne :
    (attributes1 -> Relationship HasOne id)
    -> Schema id attributes2
    -> Selection attributes2 a
    -> Selection attributes1 a
embedOne getRelationship (Schema schemaName attributes2) (Selection getSelection) =
    Selection <|
        \attributes1 ->
            let
                (Relationship fkName) =
                    getRelationship attributes1

                { attributeNames, embeds, decoder } =
                    getSelection attributes2

                parameters =
                    Parameters
                        { schemaName = schemaName
                        , attributeNames = attributeNames
                        , cardinality = One (Err True)
                        }
                        embeds
            in
            { attributeNames = []
            , embeds = [ ( fkName, parameters ) ]
            , decoder = Decode.field fkName decoder
            }


{-| -}
embedAll :
    (attributes1 -> Relationship HasMany id)
    -> Schema id attributes2
    -> Selection attributes2 a
    -> Selection attributes1 (List a)
embedAll getRelationship embeddedSchema selection =
    embedMany getRelationship embeddedSchema <|
        { select = selection
        , where_ = true
        , order = []
        , limit = Nothing
        , offset = Nothing
        }


{-| -}
embedNullable :
    (attributes1 -> Relationship HasNullable id)
    -> Schema id attributes2
    -> Selection attributes2 a
    -> Selection attributes1 (Maybe a)
embedNullable getRelationship (Schema schemaName attributes2) (Selection getSelection) =
    Selection <|
        \attributes1 ->
            let
                (Relationship fkName) =
                    getRelationship attributes1

                { attributeNames, embeds, decoder } =
                    getSelection attributes2

                parameters =
                    Parameters
                        { schemaName = schemaName
                        , attributeNames = attributeNames
                        , cardinality = One (Err True)
                        }
                        embeds
            in
            { attributeNames = []
            , embeds = [ ( fkName, parameters ) ]
            , decoder = Decode.field fkName (Decode.nullable decoder)
            }


{-| -}
embedMany :
    (attributes1 -> Relationship HasMany id)
    -> Schema id attributes2
    ->
        { select : Selection attributes2 a
        , where_ : Condition attributes2
        , order : List (Order attributes2)
        , limit : Maybe Int
        , offset : Maybe Int
        }
    -> Selection attributes1 (List a)
embedMany getRelationship (Schema schemaName attributes2) options =
    Selection <|
        \attributes1 ->
            let
                (Relationship fkOrThroughName) =
                    getRelationship attributes1

                (Selection getSelection) =
                    options.select

                { attributeNames, embeds, decoder } =
                    getSelection attributes2

                cardinality =
                    Many
                        { order = applyOrders attributes2 options.order
                        , where_ = applyCondition attributes2 options.where_
                        , limit = options.limit
                        , offset = options.offset
                        }

                parameters =
                    Parameters
                        { schemaName = schemaName
                        , attributeNames = attributeNames
                        , cardinality = cardinality
                        }
                        embeds
            in
            { attributeNames = []
            , embeds = [ ( fkOrThroughName, parameters ) ]

            -- TODO: do we need to account for fkOrThroughName here?
            , decoder = Decode.field schemaName (Decode.list decoder)
            }


{-| Simple pattern matching
-}
like : String -> (attributes -> Attribute String) -> Condition attributes
like =
    condHelper Like


{-| Case-insensitive `like`
-}
ilike : String -> (attributes -> Attribute String) -> Condition attributes
ilike =
    condHelper Ilike


{-| Equals
-}
eq : a -> (attributes -> Attribute a) -> Condition attributes
eq =
    condHelper Eq


{-| Not Equals
-}
neq : a -> (attributes -> Attribute a) -> Condition attributes
neq =
    condHelper Neq


{-| Greater than or equal to
-}
gte : a -> (attributes -> Attribute a) -> Condition attributes
gte =
    condHelper Gte


{-| Greater than
-}
gt : a -> (attributes -> Attribute a) -> Condition attributes
gt =
    condHelper Gt


{-| Less than or equal to
-}
lte : a -> (attributes -> Attribute a) -> Condition attributes
lte =
    condHelper Lte


{-| Less than
-}
lt : a -> (attributes -> Attribute a) -> Condition attributes
lt =
    condHelper Lt


{-| Is comparison
-}
is : Maybe Bool -> (attributes -> Attribute a) -> Condition attributes
is value getAttribute =
    let
        getKeyValue attributes =
            let
                (Attribute { name }) =
                    getAttribute attributes

                valueString =
                    case value of
                        Nothing ->
                            "null"

                        Just True ->
                            "true"

                        Just False ->
                            "false"
            in
            { key = name
            , value = valueString
            }
    in
    COperator
        { negated = False
        , operator = Is
        , getKeyValue = getKeyValue
        }


condHelper : Operator -> a -> (attributes -> Attribute a) -> Condition attributes
condHelper operator value getAttribute =
    let
        getKeyValue attributes =
            let
                (Attribute { toString, name }) =
                    getAttribute attributes

                valueString =
                    toString value
            in
            { key = name
            , value = valueString
            }
    in
    COperator
        { negated = False
        , operator = operator
        , getKeyValue = getKeyValue
        }


{-| -}
true : Condition attributes
true =
    CBoolean True


{-| -}
false : Condition attributes
false =
    CBoolean False


{-| Negate a Condition
-}
not : Condition attributes -> Condition attributes
not cond =
    case cond of
        COperator op ->
            COperator { op | negated = Basics.not op.negated }

        CCombine comb ->
            CCombine { comb | negated = Basics.not comb.negated }

        CBoolean boolean ->
            CBoolean (Basics.not boolean)



-- Why not just use List.member TrueCond or List.member FalseCond? Well. The
-- Condition type can have functions stored in them (in the case of COperator),
-- so I've made these helpers to avoid using the (==) operator (which is used in
-- List.member). It might be a non problem, but it's pretty easy to do this.


{-| All conditions hold true
-}
all : List (Condition attributes) -> Condition attributes
all conds =
    if List.isEmpty conds then
        CBoolean True

    else if List.any isFalseCond conds then
        CBoolean False

    else if List.all isTrueCond conds then
        -- NOTE: added this case to avoid and()
        CBoolean True

    else
        CCombine
            { negated = False
            , operator = And
            , conditions = conds
            }


{-| Any condition holds true
-}
any : List (Condition attributes) -> Condition attributes
any conds =
    if List.isEmpty conds then
        CBoolean False

    else if List.any isTrueCond conds then
        CBoolean True

    else if List.all isFalseCond conds then
        -- NOTE: added this case to avoid or()
        CBoolean False

    else
        CCombine
            { negated = False
            , operator = Or
            , conditions = conds
            }


isTrueCond : Condition attributes -> Bool
isTrueCond cond =
    case cond of
        CBoolean True ->
            True

        _ ->
            False


isFalseCond : Condition attributes -> Bool
isFalseCond cond =
    case cond of
        CBoolean False ->
            True

        _ ->
            False



-- https://www.postgresql.org/docs/current/static/queries-order.html
-- The NULLS FIRST and NULLS LAST options can be used to determine whether
-- nulls appear before or after non-null values in the sort ordering. By
-- default, null values sort as if larger than any non-null value; that is,
-- NULLS FIRST is the default for DESC order, and NULLS LAST otherwise.


{-| -}
asc : (attributes -> Attribute a) -> Order attributes
asc =
    order Asc NullsLast


{-| -}
desc : (attributes -> Attribute a) -> Order attributes
desc =
    order Desc NullsFirst


{-| -}
order : Direction -> Nulls -> (attributes -> Attribute a) -> Order attributes
order direction nulls getAttribute =
    Order
        { direction = direction
        , nulls = nulls
        , getKey = getAttribute >> (\(Attribute { name }) -> name)
        }


{-| -}
change : (attributes -> Attribute a) -> a -> Changeset attributes
change getAttribute val =
    Changeset <|
        \attributes ->
            case getAttribute attributes of
                Attribute { name, encoder } ->
                    [ ( name, encoder val ) ]


{-| -}
batch : List (Changeset attributes) -> Changeset attributes
batch values =
    Changeset <|
        \attributes ->
            values
                |> List.map (\(Changeset getKeyValues) -> getKeyValues attributes)
                |> List.concat


{-| -}
createOne :
    Schema id attributes
    ->
        { change : Changeset attributes
        , select : Selection attributes a
        }
    -> Request a
createOne (Schema name attributes) options =
    let
        (Changeset toKeyValuePairs) =
            options.change

        (Selection getSelection) =
            options.select

        { decoder, attributeNames, embeds } =
            getSelection attributes

        parameters =
            Parameters
                { schemaName = name
                , attributeNames = attributeNames
                , cardinality = One (Err True)
                }
                embeds
    in
    Create
        { parameters = parameters
        , decoder = decoder
        , value = Encode.object (toKeyValuePairs attributes)
        }


{-| -}
createMany :
    Schema id attributes
    ->
        { change : List (Changeset attributes)
        , select : Selection attributes a
        , where_ : Condition attributes
        , order : List (Order attributes)
        , limit : Maybe Int
        , offset : Maybe Int
        }
    -> Request (List a)
createMany (Schema name attributes) options =
    let
        jsonValue =
            options.change
                |> List.map (\(Changeset toKeyValuePairs) -> toKeyValuePairs attributes)
                |> List.map Encode.object
                |> Encode.list identity

        (Selection getSelection) =
            options.select

        { decoder, embeds, attributeNames } =
            getSelection attributes

        cardinality =
            Many
                { order = applyOrders attributes options.order
                , where_ = applyCondition attributes options.where_
                , limit = options.limit
                , offset = options.offset
                }

        parameters =
            Parameters
                { schemaName = name
                , attributeNames = attributeNames
                , cardinality = cardinality
                }
                embeds
    in
    Create
        { parameters = parameters
        , decoder = Decode.list decoder
        , value = jsonValue
        }


{-| -}
readOne :
    Schema id attributes
    ->
        { select : Selection attributes a
        , where_ : Condition attributes
        }
    -> Request a
readOne (Schema schemaName attributes) { select, where_ } =
    let
        (Selection getSelection) =
            select

        { attributeNames, embeds, decoder } =
            getSelection attributes

        parameters =
            Parameters
                { schemaName = schemaName
                , attributeNames = attributeNames
                , cardinality = One (applyCondition attributes where_)
                }
                embeds
    in
    Read
        { parameters = parameters
        , decoder = decoder
        }


{-| -}
readMany :
    Schema id attributes
    ->
        { select : Selection attributes a
        , where_ : Condition attributes
        , order : List (Order attributes)
        , limit : Maybe Int
        , offset : Maybe Int
        }
    -> Request (List a)
readMany (Schema schemaName attributes) options =
    let
        (Selection getSelection) =
            options.select

        { attributeNames, embeds, decoder } =
            getSelection attributes

        cardinality =
            Many
                { order = applyOrders attributes options.order
                , where_ = applyCondition attributes options.where_
                , limit = options.limit
                , offset = options.offset
                }

        parameters =
            Parameters
                { schemaName = schemaName
                , attributeNames = attributeNames
                , cardinality = cardinality
                }
                embeds
    in
    Read
        { parameters = parameters
        , decoder = Decode.list decoder
        }


{-| -}
readAll : Schema id attributes -> Selection attributes a -> Request (List a)
readAll s select =
    readMany s
        { select = select
        , where_ = true
        , order = []
        , offset = Nothing
        , limit = Nothing
        }



{-
   readPage :
       Schema id attributes
       ->
           { select : Selection attributes a
           , where_ : Condition attributes
           , order : ( Order attributes, List (Order attributes) )
           , page : Int
           , size : Int
           }
       -> Request { count : Int, data : List a }
   readPage (Schema schemaName attributes) options =
       let
           ( firstOrder, restOrder ) =
               options.order

           (Selection getSelection) =
               options.select

           { attributeNames, embeds, decoder } =
               getSelection attributes

           cardinality =
               Many
                   { order = applyOrders attributes (firstOrder :: restOrder)
                   , where_ = applyCondition attributes options.where_
                   , limit = Just options.size
                   , offset = Just ((options.page - 1) * options.size)
                   }

           parameters =
               Parameters
                   { schemaName = schemaName
                   , attributeNames = attributeNames
                   , cardinality = cardinality
                   }
                   embeds

           handleResponse response =
               let
                   countResult =
                       Dict.get "Content-Range" response.headers
                           |> Maybe.andThen (String.split "/" >> List.reverse >> List.head)
                           |> Maybe.andThen String.toInt
                           |> Result.fromMaybe "Invalid Content-Range Header"

                   jsonResult =
                       Decode.decodeString (Decode.list decoder) response.body
                           |> Result.mapError Decode.errorToString
               in
               Result.map2 (\data count -> { data = data, count = count })
                   jsonResult
                   countResult
       in
       Page
           { parameters = parameters
           , expect = Http.expectStringResponse handleResponse
           }
-}


{-| -}
updateOne :
    Schema id attributes
    ->
        { change : Changeset attributes
        , select : Selection attributes a
        , where_ : Condition attributes
        }
    -> Request a
updateOne (Schema name attributes) options =
    let
        (Changeset toKeyValuePairs) =
            options.change

        (Selection getSelection) =
            options.select

        { decoder, attributeNames, embeds } =
            getSelection attributes

        parameters =
            Parameters
                { schemaName = name
                , attributeNames = attributeNames
                , cardinality = One (applyCondition attributes options.where_)
                }
                embeds
    in
    Update
        { parameters = parameters
        , decoder = decoder
        , value = Encode.object (toKeyValuePairs attributes)
        }


{-| -}
updateMany :
    Schema id attributes
    ->
        { change : Changeset attributes
        , select : Selection attributes a
        , where_ : Condition attributes
        , order : List (Order attributes)
        , limit : Maybe Int
        , offset : Maybe Int
        }
    -> Request (List a)
updateMany (Schema name attributes) options =
    let
        (Changeset toKeyValuePairs) =
            options.change

        (Selection getSelection) =
            options.select

        { decoder, attributeNames, embeds } =
            getSelection attributes

        cardinality =
            Many
                { order = applyOrders attributes options.order
                , where_ = applyCondition attributes options.where_
                , limit = options.limit
                , offset = options.offset
                }

        parameters =
            Parameters
                { schemaName = name
                , attributeNames = attributeNames
                , cardinality = cardinality
                }
                embeds
    in
    Update
        { parameters = parameters
        , decoder = Decode.list decoder
        , value = Encode.object (toKeyValuePairs attributes)
        }


{-| -}
deleteOne :
    Schema id attributes
    ->
        { select : Selection attributes a
        , where_ : Condition attributes
        }
    -> Request a
deleteOne (Schema name attributes) { where_, select } =
    let
        (Selection getSelection) =
            select

        { decoder, embeds, attributeNames } =
            getSelection attributes

        parameters =
            Parameters
                { schemaName = name
                , attributeNames = attributeNames
                , cardinality = One (applyCondition attributes where_)
                }
                embeds
    in
    Delete
        { parameters = parameters
        , decoder = decoder
        }


{-| -}
deleteMany :
    Schema id attributes
    ->
        { select : Selection attributes a
        , where_ : Condition attributes
        , order : List (Order attributes)
        , limit : Maybe Int
        , offset : Maybe Int
        }
    -> Request (List a)
deleteMany (Schema name attributes) options =
    let
        (Selection getSelection) =
            options.select

        { decoder, embeds, attributeNames } =
            getSelection attributes

        cardinality =
            Many
                { order = applyOrders attributes options.order
                , where_ = applyCondition attributes options.where_
                , limit = options.limit
                , offset = options.offset
                }

        parameters =
            Parameters
                { schemaName = name
                , attributeNames = attributeNames
                , cardinality = cardinality
                }
                embeds
    in
    Delete
        { parameters = parameters
        , decoder = Decode.list decoder
        }



{-
   toHttpRequest : { timeout : Maybe Float, token : Maybe String, url : String } -> Request a -> Cmd a
   toHttpRequest { url, timeout, token } request =
       let
           ( authHeaders, withCredentials ) =
               case token of
                   Just str ->
                       ( [ Http.header "Authorization" ("Bearer " ++ str) ], True )

                   Nothing ->
                       ( [], False )
       in
       case request of
           Read { parameters, decoder } ->
               Http.request
                   { method = "GET"
                   , headers = parametersToHeaders parameters ++ authHeaders
                   , url = parametersToUrl url parameters
                   , body = Http.emptyBody
                   , expect = Http.expectJson decoder
                   , timeout = timeout
                   , withCredentials = withCredentials
                   }

           Page { parameters, expect } ->
               Http.request
                   { method = "GET"
                   , headers =
                       parametersToHeaders parameters
                           ++ authHeaders
                           ++ [ Http.header "Prefer" "count=exact" ]
                   , url = parametersToUrl url parameters
                   , body = Http.emptyBody
                   , expect = expect
                   , timeout = timeout
                   , withCredentials = withCredentials
                   }

           Update { parameters, decoder, value } ->
               Http.request
                   { method = "PATCH"
                   , headers = parametersToHeaders parameters ++ authHeaders
                   , url = parametersToUrl url parameters
                   , body = Http.jsonBody value
                   , expect = Http.expectJson decoder
                   , timeout = timeout
                   , withCredentials = withCredentials
                   }

           Create { parameters, decoder, value } ->
               Http.request
                   { method = "POST"
                   , headers = parametersToHeaders parameters ++ authHeaders
                   , url = parametersToUrl url parameters
                   , body = Http.jsonBody value
                   , expect = Http.expectJson decoder
                   , timeout = timeout
                   , withCredentials = withCredentials
                   }

           Delete { parameters, decoder } ->
               Http.request
                   { method = "DELETE"
                   , headers = parametersToHeaders parameters ++ authHeaders
                   , url = parametersToUrl url parameters
                   , body = Http.emptyBody
                   , expect = Http.expectJson decoder
                   , timeout = timeout
                   , withCredentials = withCredentials
                   }
-}


parametersToHeaders : Parameters -> List Http.Header
parametersToHeaders (Parameters { cardinality, attributeNames } embeds) =
    case cardinality of
        Many _ ->
            case ( attributeNames, embeds ) of
                ( [], [] ) ->
                    [ Http.header "Prefer" "return=minimal" ]

                ( _, _ ) ->
                    [ Http.header "Prefer" "return=representation" ]

        One _ ->
            -- we need to use "return=representation" in order to get the safety
            -- of only updating (or deleting) a single row. there is 1 downside
            -- and 1 strange edge case. downside: update one always returns data,
            -- even if you don't explictly select. this relates to the strange
            -- edge case: which is if you succeed () -- and the table has write
            -- only fields, the update will fail.
            [ Http.header "Accept" "application/vnd.pgrst.object+json"
            , Http.header "Prefer" "return=representation"
            ]


parametersToUrl : String -> Parameters -> String
parametersToUrl prePath ((Parameters { cardinality, schemaName, attributeNames } embeds) as parameters) =
    let
        selectQueryParameter =
            parametersToSelectQueryParameter parameters

        cardinalityQueryParameters =
            cardinalityToQueryParameters ( [], cardinality )

        topLevelQueryParameters =
            selectQueryParameter :: cardinalityQueryParameters

        embedLevelQueryParameters =
            embedsToQueryParameters embeds

        allQueryParameters =
            List.filterMap identity (topLevelQueryParameters ++ embedLevelQueryParameters)
    in
    Builder.crossOrigin prePath [ schemaName ] allQueryParameters


parametersToSelectQueryParameter : Parameters -> Maybe QueryParameter
parametersToSelectQueryParameter parameters =
    case parametersToSelectStrings parameters of
        [] ->
            Nothing

        selectStrings ->
            Just <| Builder.string "select" (String.join "," selectStrings)


parametersToSelectStrings : Parameters -> List String
parametersToSelectStrings (Parameters { attributeNames } embeds) =
    let
        embedSelectStrings =
            List.map embedToSelectString embeds

        allSelectStrings =
            attributeNames ++ embedSelectStrings
    in
    allSelectStrings


embedToSelectString : Embed -> String
embedToSelectString ( disambiguateName, Parameters { schemaName, attributeNames, cardinality } embeds ) =
    let
        embedSelectStrings =
            List.map embedToSelectString embeds

        selectStrings =
            attributeNames ++ embedSelectStrings

        selectionString =
            String.join "," selectStrings

        name =
            case cardinality of
                One _ ->
                    disambiguateName

                Many _ ->
                    schemaName ++ "." ++ disambiguateName
    in
    String.concat
        [ name
        , "("
        , selectionString
        , ")"
        ]


cardinalityToQueryParameters : ( List String, Cardinality ) -> List (Maybe QueryParameter)
cardinalityToQueryParameters ( embedPath, cardinality ) =
    case cardinality of
        Many options ->
            [ Maybe.map (ordersToQueryParameter embedPath) options.order
            , Maybe.map (offsetToQueryParameter embedPath) options.offset
            , Maybe.map (conditionToQueryParameter embedPath) (Result.toMaybe options.where_)
            , Maybe.map (limitToQueryParameter embedPath) options.limit
            ]

        One where_ ->
            [ Maybe.map (conditionToQueryParameter embedPath) (Result.toMaybe where_) ]


embedsToQueryParameters : List Embed -> List (Maybe QueryParameter)
embedsToQueryParameters embeds =
    embedsToDict embeds
        |> Dict.toList
        |> List.map cardinalityToQueryParameters
        |> List.concat


embedsToDict : List Embed -> Dict (List String) Cardinality
embedsToDict embeds =
    let
        empty =
            { embedPath = []
            , embedDict = Dict.empty
            }

        { embedDict } =
            List.foldl embedToDictHelper empty embeds
    in
    embedDict


type alias EmbedState =
    { embedPath : List String
    , embedDict : Dict (List String) Cardinality
    }


embedToDictHelper : Embed -> EmbedState -> EmbedState
embedToDictHelper ( disambiguateName, Parameters { cardinality, schemaName } embeds ) { embedPath, embedDict } =
    let
        newEmbedPath =
            case cardinality of
                Many _ ->
                    -- TODO: need to also use disambiguateName?
                    schemaName :: embedPath

                One _ ->
                    disambiguateName :: embedPath

        newEmbedDict =
            Dict.insert newEmbedPath cardinality embedDict

        newState =
            { embedPath = newEmbedPath
            , embedDict = newEmbedDict
            }
    in
    List.foldl embedToDictHelper newState embeds


operatorToString : Operator -> String
operatorToString op =
    case op of
        Eq ->
            "eq"

        Gt ->
            "gt"

        Gte ->
            "gte"

        Lt ->
            "lt"

        Lte ->
            "lte"

        Neq ->
            "neq"

        Like ->
            "like"

        Ilike ->
            "ilike"

        In ->
            "in"

        Is ->
            "is"

        Fts ->
            "fts"

        Cs ->
            "cs"

        Cd ->
            "cd"

        Ov ->
            "ov"

        Sl ->
            "sl"

        Sr ->
            "sr"

        Nxr ->
            "nxr"

        Nxl ->
            "nxl"

        Adj ->
            "adj"



{-
   TOP LEVEL:
   roles.character=in.Chico,Harpo,Groucho
   \___/ \_______/ \_/ \______________/
     |       |      |         |
   schema   attr   oper      value


   NESTED:
   and=(grade.gte.90,student.is.true)

   NOT:
   a=not.eq.2
   and=id.not.eq.1 <-- nested not is after attribute
   not.and=(a.gte.0,a.lte.100) <-- not or/and is before
-}


applyCondition : attributes -> Condition attributes -> Result Bool Condition_
applyCondition attributes cond =
    case cond of
        COperator { negated, operator, getKeyValue } ->
            let
                { key, value } =
                    getKeyValue attributes
            in
            Ok <|
                COperator_
                    { negated = negated
                    , operator = operator
                    , key = key
                    , value = value
                    }

        CCombine { negated, operator, conditions } ->
            Ok <|
                CCombine_
                    { negated = negated
                    , operator = operator
                    , conditions = List.filterMap (applyCondition attributes >> Result.toMaybe) conditions
                    }

        CBoolean boolean ->
            Err boolean


applyOrder : attributes -> Order attributes -> Order_
applyOrder attributes (Order { direction, nulls, getKey }) =
    Order_
        { direction = direction
        , nulls = nulls
        , key = getKey attributes
        }


applyOrders : attributes -> List (Order attributes) -> Maybe Orders
applyOrders attributes orders =
    case orders of
        [] ->
            Nothing

        first :: rest ->
            let
                firstParam =
                    applyOrder attributes first

                restParams =
                    List.map (applyOrder attributes) rest
            in
            Just ( firstParam, restParams )


directionToString : Direction -> String
directionToString direction =
    case direction of
        Asc ->
            "asc"

        Desc ->
            "desc"


nullsToString : Nulls -> String
nullsToString nulls =
    case nulls of
        NullsFirst ->
            "nullsfirst"

        NullsLast ->
            "nullslast"


ordersToQueryParameter : List String -> Orders -> QueryParameter
ordersToQueryParameter embedPath ( firstOrder, restOrders ) =
    let
        orders =
            firstOrder :: restOrders

        queryKey =
            ("order" :: embedPath)
                |> List.reverse
                |> String.join "."

        orderDataToString (Order_ { direction, nulls, key }) =
            String.join "."
                [ key
                , directionToString direction
                , nullsToString nulls
                ]

        queryValue =
            orders
                |> List.map orderDataToString
                |> String.join ","
    in
    Builder.string queryKey queryValue


conditionToQueryParameter : List String -> Condition_ -> QueryParameter
conditionToQueryParameter embedPath condition =
    case condition of
        COperator_ { negated, operator, key, value } ->
            let
                queryKey =
                    (key :: embedPath)
                        |> List.reverse
                        |> String.join "."

                queryValue =
                    String.concat
                        [ if negated then
                            "not."

                          else
                            ""
                        , operatorToString operator
                        , "."
                        , value
                        ]
            in
            Builder.string queryKey queryValue

        CCombine_ { negated, operator, conditions } ->
            let
                binOpString =
                    binaryLogicalOperatorToString operator

                queryKey =
                    (binOpString :: embedPath)
                        |> List.reverse
                        |> String.join "."

                negatedQueryKey =
                    if negated then
                        "not." ++ queryKey

                    else
                        queryKey

                queryValue =
                    conditions
                        |> List.map conditionToString
                        |> String.join ","
            in
            Builder.string negatedQueryKey ("(" ++ queryValue ++ ")")


limitToQueryParameter : List String -> Int -> QueryParameter
limitToQueryParameter embedPath limit =
    let
        queryKey =
            ("limit" :: embedPath)
                |> List.reverse
                |> String.join "."

        queryValue =
            String.fromInt limit
    in
    Builder.string queryKey queryValue


offsetToQueryParameter : List String -> Int -> QueryParameter
offsetToQueryParameter embedPath offset =
    let
        queryKey =
            ("offset" :: embedPath)
                |> List.reverse
                |> String.join "."

        queryValue =
            String.fromInt offset
    in
    Builder.string queryKey queryValue


conditionToString : Condition_ -> String
conditionToString condition =
    case condition of
        COperator_ { negated, operator, key, value } ->
            String.concat
                [ key
                , "."
                , if negated then
                    "not."

                  else
                    ""
                , operatorToString operator
                , "."
                , value
                ]

        CCombine_ { negated, operator, conditions } ->
            String.concat
                [ if negated then
                    "not."

                  else
                    ""
                , binaryLogicalOperatorToString operator
                , "("
                , conditions
                    |> List.map conditionToString
                    |> String.join ","
                , ")"
                ]


binaryLogicalOperatorToString : BinaryLogicalOperator -> String
binaryLogicalOperatorToString binop =
    case binop of
        And ->
            "and"

        Or ->
            "or"
