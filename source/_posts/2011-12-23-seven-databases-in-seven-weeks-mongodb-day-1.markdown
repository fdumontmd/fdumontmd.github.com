---
layout: post
title: "Seven Databases in Seven Weeks MongoDB Day 1"
date: 2011-12-23 08:19
comments: true
categories: [Books]
tags: [7databases7weeks, databases, mongodb]
series: "Seven Databases in Seven Weeks"
---

It has been ... a little over a week since I closed with HBase so it
is time to move to the next database in the program:
[MongoDB](http://www.mongodb.org/). It is a so-called
[document oriented database](http://en.wikipedia.org/wiki/Document-oriented_database),
relying on Javascript and JSON like Riak but, MongoDB, unlike Riak,
has a built-in support for querying, and can also perform partial
updates.

Fittingly, this first day is about CRUD and queries.

<!--more-->

### CRUD in MongoDB

Unlike Riak, MongoDB makes a difference between Create and Update
operations. It is not possible to insert documents with the same id
twice in the same collection:

```
> db.test.insert( { _id: 'one', value: 1 } )
> db.test.find()
{ "_id" : "one", "value" : 1 }
> db.test.insert( { _id: 'one', value: 2 } )
E11000 duplicate key error index: book.test.$_id_  dup key: { : "one" }
```

Like in SQL, the Read, Update and Delete operations all operate on the
result of a search. And because the criteria query for a search is
just a JSON document, it is possible to reuse it in different
contexts. Combined with JavaScript as a query/shell language, this
makes a very flexible combination.

### Reaching into objects

As documents are naturally nested, it is important to know how to
refer to nested attributes and how to use them in queries.

Using the JSON nesting notation is possible but does not have the
expected semantic (unless you change your expectations):

```
> db.towns.find( { mayor: { name: 'Sam Adams' } }, {name: 1} )
>
```

The query above returns nothing, despite the fact that there is a town
whose mayor is named 'Sam Adams'. The problem here is that this
specific notation will match documents in the collection whose `mayor`
attribute has the exact value `{ name: 'Sam Adams' }`, rather than
merely those whose `mayor` attribute has a sub-attribute `name` with
the `Sam Adams` value (and possible other sub-attributes, a
possibility that the first query denies).

The correct form for the query is
```
> db.towns.find( { 'mayor.name' : 'Sam Adams' }, {name: 1})
{ "_id" : ObjectId("4eed55b0ef971f5317e68e91"), "name" : "Portland" }
>
```

[This documentation](http://www.mongodb.org/display/DOCS/Dot+Notation+%28Reaching+into+Objects%29)
from the official website was very useful to me in clarifying this aspect.

### The [`$not`](http://www.mongodb.org/display/DOCS/Advanced+Queries#AdvancedQueries-Metaoperator%3A%7B%7B%24not%7D%7D) operator

I first got curious about this operator because the example from the
book did not work. It generated an error in MongoDB 2.0.2 (the latest
at the time of writing).

{% codeblock lang:javascript %}
db.countries.find(
    {
        'exports.foods' : {
            $not : { name : 'burgers' }
        }
    },
    { _id : 0, name : 1 }
)
{% endcodeblock %}

The error is

```
error: { "$err" : "invalid use of $not", "code" : 13034 }
```

From the
[source code](https://github.com/mongodb/mongo/blob/master/src/mongo/db/queryutil.cpp),
it seems $not cannot be applied to a (implicit) equal operator. So the
book code should be using `$ne`:

{% codeblock lang:javascript %}
db.countries.find( { 'exports.foods.name': { $ne: 'burgers' } } )
{% endcodeblock %}

It does not help that the online shell (accessible from the 'Try it
out' link on [http://www.mongodb.org/](http://www.mongodb.org/)) does
not return an error for the book query. It does not return anything
at all, so it is still not correct.

Still, even with 2.0.2, this query:
```
> db.countries.find( {$not: { name: 'United States' } } )
>
```

is accepted but does not return every country but US. Yet again it
returns nothing.

As far as I can tell, this query is actually incorrect (the `$not`
operator should be applied to an operator, not a match), so its value
is irrelevant, but it is worrying that the parser allows this code.

The general problem is that the semantic of `$not` is not exactly
the same as the similar operator from logic. So basic logic reasoning
cannot help.

I suppose this is just something to keep in mind: be wary of using
`$not`.

## Exercises

### The online MongoDB documentation

is [here](http://www.mongodb.org/display/DOCS/Manual).

### Query using regular expressions

As explained
[here](http://www.mongodb.org/display/DOCS/Advanced+Queries#AdvancedQueries-RegularExpressions),
MongoDB supports two notations: a direct JavaScript regexp object
using `/.../` and slightly more verbose but more flexible `$regexp`
operator. The syntax for the regular expression in both cases is the
same.

### MongoDB drivers

The [list of drivers](http://www.mongodb.org/display/DOCS/Drivers).

### Print a JSON document containing `{ "hello" : "world" }`

The `tojson` function can display (and pretty-print) JSON documents:

{% codeblock lang:javascript %}
tojson({ "hello": "world" });
{% endcodeblock %}

### Find a town by case insensitive regular expression search

This would be the `i` option to the regular expression:

{% codeblock lang:javascript %}
db.towns.find({name: /new/i})
{% endcodeblock %}

### Find all cities whose names contain an ‘e’, and are famous for food or beer.

A good use for the
[`$in`](http://www.mongodb.org/display/DOCS/Advanced+Queries#AdvancedQueries-%24in)
operator:

{% codeblock lang:javascript %}
db.towns.find(
    { name : /e/,
      famous_for :
      { $in: ['food', 'beer'] } } )
{% endcodeblock %}

or the equivalent but verbose:

{% codeblock lang:javascript %}
db.towns.find(
    { name: /e/,
      $or: [{famous_for : 'food'},
            {famous_for: 'beer'} ] } )
{% endcodeblock %}

### Find all countries that do not export tasty bacon

To find countries that export tasty bacon, one has to use
[`$elemMatch`](http://www.mongodb.org/display/DOCS/Advanced+Queries#AdvancedQueries-%24elemMatch). It
should not come as a surprise that it is used here as well, but with
the dreaded `$not`:

{% codeblock lang:javascript %}
db.countries.find(
    {'exports.foods':
     { $not:
       {$elemMatch:
        { name: 'bacon',
          tasty: true } } } }, {name: 1} )
{% endcodeblock %}

produces

{% codeblock lang:javascript %}
{ "_id" : "ca", "name" : "Canada" }
{ "_id" : "mx", "name" : "Mexico" }
{% endcodeblock %}

(I had not deleted Canada from the database).

The seemingly equivalent

{% codeblock lang:javascript %}
db.countries.find(
    { $not:
      {'exports.foods':
       { $elemMatch:
         { name: 'bacon',
           tasty: true } } } })
{% endcodeblock %}

will not work. It does not return anything. See above my findings on `$not`.

### Create a database `blogger` with a collection of `articles`

#### A new database:

Creating a new database is extremely easy: just name it when you
launch the `mongo` shell command:

```
mongo blogger
```

#### A new collection:

Like for the database, a collection is created just by inserting
something in it:

{% codeblock lang:javascript %}
db.articles.insert(
    { name: 'Frederic', email: 'my@email.org',
      creation: new Date(),
      test: 'Neque porro quisquam est qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit...' } )
{% endcodeblock %}

#### Update the article with an array of comments

With the operator
[`$set`](http://www.mongodb.org/display/DOCS/Updating#Updating-%24set),
I can specify just what I need to update, not the whole document as in
Riak:

{% codeblock lang:javascript %}
db.articles.update(
    {"_id" : ObjectId("4ef3c78c7d67f191b06653e4") },
    {$set: { comments:
             [ {author: 'Alice', text: 'Me too'},
               {author: 'Bob', text: 'Me three'},
               {author: 'Carol', text: 'I know all your secret' } ]
           } } )
{% endcodeblock %}

I can check the inserted value with `db.articles.findOne()` (without
arguments it returns the first element in the collection, and pretty
prints it):

{% codeblock lang:javascript %}
{
        "_id" : ObjectId("4ef3c78c7d67f191b06653e4"),
        "comments" : [
                {
                        "author" : "Alice",
                        "text" : "Me too"
                },
                {
                        "author" : "Bob",
                        "text" : "Me three"
                },
                {
                        "author" : "Carol",
                        "text" : "I know all your secrets"
                }
        ],
        "creation" : ISODate("2011-12-23T00:13:00.636Z"),
        "email" : "my@email.org",
        "name" : "Frederic",
        "test" : "Neque porro quisquam est qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit..."
}
{% endcodeblock %}

Tomorrow will cover indexing, and more advanced uses of the data (such
as MapReduce).
