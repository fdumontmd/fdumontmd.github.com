---
layout: post
title: "Seven Databases in Seven Weeks mongoDB Day 1"
date: 2011-12-23 08:19
comments: true
categories: [Books]
tags: [7databases7weeks, databases, mongodb]
series: "Seven Databases in Seven Weeks"
---

It has been ... a little over a week since I closed with HBase so it
is time to move to the next database in the program:
[mongoDB](http://www.mongodb.org/). It is a so-called [document
oriented database](http://en.wikipedia.org/wiki/Document-oriented_database),
relying on Javascript and JSON like Riak but, mongoDB, unlike Riak,
has a built-in support for querying, and can also perform partial
updates.

Fittingly, this first day is about CRUD and queries.

<!--more-->

### Reaching into objects

As documents are naturally nested, it is important to know how to
refer to nested attributes and how to use them in queries.


talk about 'bla.bla' vs 'bla' : { 'bla' : ...}

### The [`$not`](http://www.mongodb.org/display/DOCS/Advanced+Queries#AdvancedQueries-Metaoperator%3A%7B%7B%24not%7D%7D) operator

I first got curious about this operator because the example from the
book did not work. It generated an error.

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

The query that achieves the intended result it

{% codeblock lang:javascript %}
db.countries.find( { 'exports.foods.name': { $ne: 'burgers' } } )
{% endcodeblock %}


db.countries.find( {$not: { name: 'United States' } } )

does not return every country but US. It returns nothing...

In the online shell, the behaviour is still not easy to figure out:
the code from the book does run, but returns nothing.

From the [source code](https://github.com/mongodb/mongo/blob/master/db/queryutil.cpp), it seems $not cannot be applied to a (implicit)
equal operator. So the book code should be


(this code does work).

However, it is still not easy to figure out how to use $not
properly. Basic logic fails here.

## Exercises

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

Curiously, the seemingly equivalent

{% codeblock lang:javascript %}
db.countries.find(
    { $not:
      {'exports.foods':
       { $elemMatch:
         { name: 'bacon',
           tasty: true } } } })
{% endcodeblock %}

will not work. It does not return anything.

As far as I can tell, this second query is actually incorrect (the
`$not` operator should be applied to an operator, not a match), so its
value is irrelevant. But it is worrying that the parser allows this
code.

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
