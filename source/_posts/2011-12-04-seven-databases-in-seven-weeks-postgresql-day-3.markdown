---
layout: post
title: "Seven Databases in Seven Weeks PostgreSQL Day 3"
date: 2011-12-04 20:19
comments: true
categories: [Books]
tags: [7databases7weeks, databases, postgresql]
series: "Seven Databases in Seven Weeks"
---
Third and final day with PostgreSQL. Today all kind of text and other fancy searches are looked at. Here the SQL standard is resolutely left behind, as pretty much everything is PostgreSQL specific.

PostgreSQL manages advanced index data structures that allow it to efficiently query data using something better than basic comparisons. It especially shines in its handling of geospatial data (searching by distance is a non-trivial problem), but has many options for text searches as well.
<!--more-->

Now, it can be easy to get carried away and try to use PostgreSQL for everything, even when a superior alternative option exists. If I had any need for a full text search, I would also look at [Lucene](http://lucene.apache.org/java/docs/index.html) and related options. But PostgreSQL can still provide an easy, integrated solution for many situations.

### On indexes

Indexes are not like pixie dust: you cannot just add some and hope all your performance problems will go away (as I learned the hard way). Benchmarks and the [`EXPLAIN`](http://www.postgresql.org/docs/current/static/sql-explain.html) command must be used to confirm the improvements brought by any index.

This section is based on the first beta version of the book. I hope they fill clarify this part in the final version.

First, let's look a bit more at the fist index introduced today:

{% codeblock Text Pattern index lang:sql %}
CREATE INDEX movies_title_pattern ON movies (title text_pattern_ops);
{% endcodeblock %}

The book states that is creates an index for pattern matching. Think about it for a few seconds, try to imagine what it would look like... Yes, the solution is not trivial, so being able to index for pattern matching sounds like magic. Or pixie dust. But does it work?

```
book=# EXPLAIN SELECT title FROM movies WHERE title ILIKE 'stardust_%';
                       QUERY PLAN                        
---------------------------------------------------------
 Seq Scan on movies  (cost=0.00..160.76 rows=1 width=15)
   Filter: (title ~~* 'stardust_%'::text)
(2 rows)
```

It seems that no, it does not work. The optimizer proposes to use a sequential scan, not an index.

The problem here is that `ILIKE` is not really supported by the optimizer. It is a PostgreSQL extension which, while useful, is not that well integrated. The standard (and SQL compliant) way to do a case insensitive search is:

```
book=# EXPLAIN SELECT title FROM movies WHERE lower(title) LIKE 'stardust_%';
                        QUERY PLAN                        
----------------------------------------------------------
 Seq Scan on movies  (cost=0.00..167.91 rows=14 width=15)
   Filter: (lower(title) ~~ 'stardust_%'::text)
(2 rows)
```

Ok, not quite there yet. But that's normal. We are not using the column directly, so an index on the original values is not going to work. However PostgreSQL has a very nice feature called functional index: it is possible to create an index on the result of a function. Let's drop the original index and create one with `lower(title)` as the indexed value:

{% codeblock Case insensitive Pattern index lang:sql %}
DROP INDEX movies_title_pattern;
CREATE INDEX movies_title_pattern ON movies (lower(title) text_pattern_ops);
{% endcodeblock %}

Does it look better now?

```
book=# EXPLAIN SELECT title FROM movies WHERE lower(title) LIKE 'stardust_%';
                                             QUERY PLAN                                             
----------------------------------------------------------------------------------------------------
 Bitmap Heap Scan on movies  (cost=4.40..46.55 rows=14 width=15)
   Filter: (lower(title) ~~ 'stardust_%'::text)
   ->  Bitmap Index Scan on movies_title_pattern  (cost=0.00..4.40 rows=14 width=0)
         Index Cond: ((lower(title) ~>=~ 'stardust'::text) AND (lower(title) ~<~ 'stardusu'::text))
(4 rows)
```

Yes, that's better. What about `ILIKE`?

```
book=# EXPLAIN SELECT title FROM movies WHERE title ILIKE 'stardust_%';
                       QUERY PLAN                        
---------------------------------------------------------
 Seq Scan on movies  (cost=0.00..160.76 rows=1 width=15)
   Filter: (title ~~* 'stardust_%'::text)
(2 rows)
```

No, it still cannot use the index.

What about the regular expression query?

```
book=# EXPLAIN SELECT COUNT(*) FROM movies WHERE title !~* '^the.*';
                           QUERY PLAN                            
-----------------------------------------------------------------
 Aggregate  (cost=166.32..166.33 rows=1 width=0)
   ->  Seq Scan on movies  (cost=0.00..160.76 rows=2225 width=0)
         Filter: (title !~* '^the.*'::text)
(3 rows)
```

No, it does not like the case insensitive operator. Would a `lower(title)` conversion work?

```
book=# EXPLAIN SELECT COUNT(*) FROM movies WHERE lower(title) !~ '^the.*';
                           QUERY PLAN                            
-----------------------------------------------------------------
 Aggregate  (cost=175.03..175.04 rows=1 width=0)
   ->  Seq Scan on movies  (cost=0.00..167.91 rows=2847 width=0)
         Filter: (lower(title) !~ '^the.*'::text)
(3 rows)
```

Still not. Well, we're not looking for a particular pattern, but for everything else. A negation is not easy to propagate through the optimizer logic, so it should not be surprising that it still cannot use the index. But this (almost) similar query does:

```
book=# EXPLAIN SELECT COUNT(*) FROM movies WHERE lower(title) ~ '^the.*';
                                           QUERY PLAN                                           
------------------------------------------------------------------------------------------------
 Aggregate  (cost=46.59..46.60 rows=1 width=0)
   ->  Bitmap Heap Scan on movies  (cost=4.40..46.55 rows=14 width=0)
         Filter: (lower(title) ~ '^the.*'::text)
         ->  Bitmap Index Scan on movies_title_pattern  (cost=0.00..4.40 rows=14 width=0)
               Index Cond: ((lower(title) ~>=~ 'the'::text) AND (lower(title) ~<~ 'thf'::text))
(5 rows)
```

Interestingly, the trigram based index can help `ILIKE` queries as explained [here](http://www.postgresonline.com/journal/archives/212-PostgreSQL-9.1-Trigrams-teaching-LIKE-and-ILIKE-new-tricks.html) (in general, the [Postgres OnLine Journal](http://www.postgresonline.com/) is a very good resource on PostgreSQL more advanced features).

But the general conclusion and take-home lesson is that adding indexes without checking their impact on queries is more than useless: it adds cost on data creation and update, with not compensation at query time.

Exercises
---------

### Contributed Packages

The contributed packages shipped with PostgreSQL are documented [here](http://www.postgresql.org/docs/current/static/contrib.html).

### POSIX Regex

The Wikipedia [page](http://en.wikipedia.org/wiki/Regular_expression#Syntax) on the topic is already very good.

### Stored Procedure based movie recommendation

I'm assuming the name of an actor or movie can have errors (after all, end users are known to make and cause errors). The first step will be to identify whether the name is closer to an actor's name or a movie's name. The second step will use existing queries to either propose 5 movies from the same actor if the name was closer to an actor's name or 5 similar movies (using the cube bounding technique) if the name was closer to a movie's.

The use of `UNION` over several strategies to identify either movies or actors gives a lot of freedom. I use the `levenshtein` function to select the best match identified by each strategy.

Also, because the full text search query must have a specific format (for instance, each search term must be separated by `&`), I use a few text replacement functions to clean up the input search.

Finally, the return value is a `setof` `movies`. The meaning is that I will return rows that have the same type as rows from the `movies` table. If the query was simple, I could just use the `SELECT` statement as the body for the function, but as I have to chose between queries, I use the [`RETURN NEXT`]() command instead. Explanations can be found in the fine PostgreSQL [manual](http://www.postgresql.org/docs/current/static/plpgsql-control-structures.html).

{% include_code suggestMovies function lang:sql 7d7w/postgresql/suggest_movies.sql %}

Some testing:

```
book=# select * from suggest_movies('Ben Aflk');
 movie_id |       title       |                          genre                          
----------+-------------------+---------------------------------------------------------
       22 | Armageddon        | (5, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      146 | Good Will Hunting | (0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      476 | Forces of Nature  | (0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      514 | Dogma             | (0, 0, 0, 5, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0)
      609 | Chasing Amy       | (0, 0, 0, 5, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
(5 rows)

book=# select * from suggest_movies('Broos Weells');
 movie_id |       title       |                          genre                           
----------+-------------------+----------------------------------------------------------
        6 | The Fifth Element | (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0)
        9 | Twelve Monkeys    | (0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 7, 0, 7, 0)
       22 | Armageddon        | (5, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      171 | Die Hard          | (7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      230 | Pulp Fiction      | (0, 0, 0, 12, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 0)
(5 rows)

book=# select * from suggest_movies('war star');
 movie_id |                     title                      |                          genre                          
----------+------------------------------------------------+---------------------------------------------------------
      532 | Star Wars: Episode V - The Empire Strikes Back | (0, 7, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 2, 10, 0, 0, 0)
     2862 | Avatar                                         | (0, 7, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 5, 10, 0, 0, 0)
     1357 | Explorers                                      | (0, 5, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 5, 0, 0, 0)
      325 | Krull                                          | (0, 5, 0, 0, 0, 0, 0, 5, 0, 5, 0, 0, 0, 0, 7, 0, 0, 0)
      193 | E.T. The Extra-Terrestrial                     | (0, 5, 0, 0, 0, 0, 0, 5, 0, 5, 0, 0, 0, 0, 5, 0, 0, 0)
(5 rows)
```

Groovy.

Wrapping Up PostgreSQL
----------------------

Well, not really wrapping it up. I'll keep using it, if only for fun. This database is powerful, mature, well documented, and extremely flexible over the domain of relational modeling. It is possible to define user defined types with arbitrary content, and fancy indexing. If you find an obscure academic paper describing a exotic indexing for a new datatype, chances are that PostgreSQL will support it, given enough C programming.

The comments from the book on this database are very fair, and I would strongly recommend anyone to give it a good and honest evaluation.