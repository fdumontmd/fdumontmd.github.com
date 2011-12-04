---
layout: post
title: "Seven Databases in Seven Weeks PostgreSQL Day 2"
date: 2011-12-03 14:54
comments: true
categories: [Books]
tags: [7databases7weeks, databases, postgresql]
series: "Seven Databases in Seven Weeks"
---
Second day with PostgreSQL, this time to discuss advanced queries, stored procedures, and rewriting rules.

The relational model was designed to make it easy to extract meaningful information from the database (although here the operative word is meaningful rather than easy). Aggregations pretty much require a schema to return anything worth the effort (and if you don't believe it, what exactly is the average value of 1, blue and 3?).

Stored procedures can help to move some business intelligence from the main application to the database. Whether it is a good idea is an open question (and I think the knee-jerk negative answer is too often guided by ignorance rather than experience), but the idea of having enough rules in the database that multiple applications can connect to it safely (or safely enough) is worth considering: it enables other applications, some of which might be incompatible with the main business application, to use the business data (many reporting and ETL solutions might fall into this category). The choice is between control and openness (and which one is correct depends on the situation).

Finally, rewriting rules is a less common feature of SQL databases, but essentially it allows the database designer to create updatable views, implement versioning on specific tables, and so on.
<!--more-->
I have a small peeve with the book so far: all too often the authors rely on implicit column ordering when manipulating data. I happen to have a different order for the columns of `events` (which was created as an exercise [yesterday](/blog/2011/12/03/seven-databases-in-seven-weeks-postgresql-day-1/)), so many `INSERT` examples no longer work.

But in general, it is good to specify the columns in an `INSERT`, as in:
{% codeblock INSERT with explicit columns lang:sql %}
INSERT INTO events (title, starts, ends, venue_id) 
  VALUES ('Your Favorite Band', '2012-02-06 21:00', '2012-02-06 23:00', (
		SELECT venue_id FROM venues WHERE name = 'Crystal Ballroom' )
  );
{% endcodeblock %}

And to create 'My Place':

{% codeblock INSERT My Place lang:sql %}
INSERT INTO countries (country_code, country_name) VALUES ('jp', 'Japan');
INSERT INTO cities (name, postal_code, country_code) VALUES ('Shinjuku', '160-0022', 'jp');
INSERT INTO venues (name, type, postal_code, country_code) VALUES ('My Place', 'private', '160-0022', 'jp');
{% endcodeblock %}

Finally, to add the new events:

{% codeblock INSERT new events lang:sql %}
INSERT INTO events (title, starts, ends, venue_id) VALUES
  ('Steven King', '2012-02-26 21:00:00', '2012-02-26 23:00:00', 
    (SELECT venue_id FROM venues WHERE name = 'Powell''s Books')),
  ('Dinner with Mom', '2012-02-26 18:00:00', '2012-02-26 20:30:00', 
    (SELECT venue_id FROM venues WHERE name = 'My Place')),
  ('Valentine''s Day', '2012-02-14 00:00:00', '2012-02-14 23:59:00', NULL);
{% endcodeblock %}

This way, there is never any ambiguity as to what is inserted.

### Window functions

The book covers window functions, but in this first beta version of the book at least, the explanation is not very illuminating. 

Basically, [window functions](http://www.postgresql.org/docs/current/static/functions-window.html) are a generalization of aggregate functions. Aggregate functions operates on a range of rows selected by a `GROUP BY` clause. For each group, there will be only one row, where columns are either grouped by columns, or aggregates.

Window functions also operate on a range of rows, but there is one range for each row in the filtered table. The range can be created in a way similar to `GROUP BY` (using the `PARTITION OVER` clause), but can also be created by taking all the rows up to the current one, or 2 rows before, 2 rows after, and the current one, ... Such a range is called a window. There are [many ways](http://www.postgresql.org/docs/current/static/sql-expressions.html#SYNTAX-WINDOW-FUNCTIONS) to define them.

For instance, say we want to know, for each events, how many events have happened (including the current one), we can try:
```
book=# SELECT title, starts, COUNT(*) OVER (ORDER BY starts) FROM events;
       title        |       starts        | count 
--------------------+---------------------+-------
 Your Favorite Band | 2012-02-06 21:00:00 |     1
 Valentine's Day    | 2012-02-14 00:00:00 |     2
 My Book Signing    | 2012-02-15 17:30:00 |     3
 Dinner with Mom    | 2012-02-26 18:00:00 |     4
 Steven King        | 2012-02-26 21:00:00 |     5
 April Fools Day    | 2012-04-01 00:00:00 |     6
 House Party        | 2012-05-03 23:00:00 |     7
 Christmas Day      | 2012-12-25 00:00:00 |     8
 Valentine's Day    | 2013-02-14 00:00:00 |     9
(9 rows)
```

The `OVER` does not specify a `PARTITION`, but an `ORDER`, which means that the `COUNT(*)` function operates on all the rows from first one to current one (ordered by the `starts` column). Actually, the `COUNT(*)` function is the same as the (proper) window function `RANK`:
```
book=# SELECT title, starts, RANK() OVER (ORDER BY starts) FROM events;
       title        |       starts        | rank 
--------------------+---------------------+------
 Your Favorite Band | 2012-02-06 21:00:00 |    1
 Valentine's Day    | 2012-02-14 00:00:00 |    2
 My Book Signing    | 2012-02-15 17:30:00 |    3
 Dinner with Mom    | 2012-02-26 18:00:00 |    4
 Steven King        | 2012-02-26 21:00:00 |    5
 April Fools Day    | 2012-04-01 00:00:00 |    6
 House Party        | 2012-05-03 23:00:00 |    7
 Christmas Day      | 2012-12-25 00:00:00 |    8
 Valentine's Day    | 2013-02-14 00:00:00 |    9
(9 rows)
```

What about computing the order of each events, but by year? Nothing easier:
```
book=# SELECT title, starts, RANK() OVER (PARTITION BY extract(year from starts) ORDER BY starts) FROM events;
       title        |       starts        | rank 
--------------------+---------------------+------
 Your Favorite Band | 2012-02-06 21:00:00 |    1
 Valentine's Day    | 2012-02-14 00:00:00 |    2
 My Book Signing    | 2012-02-15 17:30:00 |    3
 Dinner with Mom    | 2012-02-26 18:00:00 |    4
 Steven King        | 2012-02-26 21:00:00 |    5
 April Fools Day    | 2012-04-01 00:00:00 |    6
 House Party        | 2012-05-03 23:00:00 |    7
 Christmas Day      | 2012-12-25 00:00:00 |    8
 Valentine's Day    | 2013-02-14 00:00:00 |    1
(9 rows)
```

Ok, this is not very fancy. But with numeric data, window functions are more powerful. Lets say we collect daily measures into a new table:

{% codeblock Window function example lang:sql %}
CREATE TABLE observation (
   day timestamp PRIMARY KEY,
   measure int NOT NULL
);

INSERT INTO observation (day, measure) (SELECT generate_series(1, 100) + date '2011-12-01' AS day, (random() * 100)::int as measure);
{% endcodeblock %}

We could use the usual aggregate functions, for instance `AVG` and `SUM`:

```
book=# SELECT AVG(measure), SUM(measure) FROM observation;
         avg         | sum  
---------------------+------
 54.9600000000000000 | 5496
(1 row)
```

We have pretty much all the details. If there's any trend, we would not see it. But with window functions, it is possible to compute running averages (and actually, two different ones):

{% codeblock Window function example lang:sql %}
SELECT day, AVG(measure) OVER (ROWS BETWEEN 2 PRECEDING AND 2 FOLLOWING) as short_avg, 
  AVG(measure) OVER (ROWS BETWEEN 5 PRECEDING AND 5 FOLLOWING) as long_avg, 
  SUM(measure) OVER (ORDER BY day) as running_sum FROM observation;
{% endcodeblock %}

(results omitted because they are too long). Here, the `AVG(measure)` is applied to a window of either 5 rows for `short_avg` or 11 rows for `long_avg`, and there's a running sum in `running_sum`.

Pretty cool, I'd say.

### Rules

[Rules](http://www.postgresql.org/docs/current/static/sql-createrule.html) are very useful as well. Here's how I'd implement the `INSERT` on `holidays`:

{% codeblock INSERT on holidays lang:sql %}
CREATE RULE insert_holidays AS ON INSERT TO holidays DO INSTEAD 
  INSERT INTO events (title, starts, ends, colors) 
  values (NEW.name, NEW.date, NEW.date+interval '23 hour 59 minutes', NEW.colors);
{% endcodeblock %}

Note the syntax to manipulate dates and timestamp. It is fairly readable and compact enough.

Exercises
---------

### Aggregate Functions

The aggregate functions are documented [here](http://www.postgresql.org/docs/current/static/functions-aggregate.html), while the window functions are [here](http://www.postgresql.org/docs/current/static/functions-window.html). 

### GUI

Honestly, I don't really use any. `psql` is really powerful. For those of the GUI persuasion, there are a few [options](http://wiki.postgresql.org/wiki/Community_Guide_to_PostgreSQL_GUI_Tools).

### DELETE Rule

{% codeblock DELETE rule on venues lang:sql %}
CREATE RULE delete_venue AS ON DELETE TO venues DO INSTEAD 
  UPDATE venues SET active = false WHERE venue_id = OLD.venue_id;
{% endcodeblock %}

With this rule in place, deleting from `venues` now set the `active` flag to false:

```
book=# DELETE FROM venues WHERE name = 'My Place';
DELETE 0
book=# SELECT * FROM venues;
 venue_id |       name       | street_address |  type   | postal_code | country_code | active 
----------+------------------+----------------+---------+-------------+--------------+--------
        1 | Crystal Ballroom |                | public  | 97205       | us           | t
        2 | Powell's Books   |                | public  | 97205       | us           | t
        5 | Run's House      |                | public  | 97205       | us           | t
        4 | My Place         |                | private | 160-0022    | jp           | f
(4 rows)
```

### generate_series in crosstab

The documentation for [`crosstab`](http://www.postgresql.org/docs/current/static/tablefunc.html) has already an example for using [`generate_series`](http://www.postgresql.org/docs/current/static/functions-srf.html):

{% codeblock generate_series in crosstab lang:sql %}
SELECT * FROM crosstab(
'SELECT extract(year from starts) as year,
extract(month from starts) as month, count(*) FROM events
GROUP BY year, month',
  'SELECT m FROM generate_series(1, 12) m'
) AS (
year int,
jan int, feb int, mar int, apr int, may int, jun int, jul int, aug int, sep int, oct int, nov int, dec int
) ORDER BY YEAR;
{% endcodeblock %}

The output remains the same:

```
 year | jan | feb | mar | apr | may | jun | jul | aug | sep | oct | nov | dec 
------+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----
 2012 |     |   5 |     |   1 |   1 |     |     |     |     |     |     |   1
 2013 |     |   1 |     |     |     |     |     |     |     |     |     |    
(2 rows)
```

### Weekly pivot

The hardest, perhaps, is to find how to convince PostgreSQL to apply [`div`](http://www.postgresql.org/docs/current/static/functions-math.html#FUNCTIONS-MATH-FUNC-TABLE) to the passed arguments, but a bit of type declaration using `::` does the trick. Also, `crosstab` is not overly smart, so the `month` and `week` columns must be in order, otherwise the counts for the two February (one in 2012 and one in 2013) are different:

{% codeblock Weekly pivot lang:sql %}
SELECT * FROM crosstab(
'SELECT extract(month from starts) as month, 
 div(extract (day from starts)::int, 7) + 1 as week, count(*) FROM events
GROUP BY month, week ORDER BY month, week',
  'SELECT m FROM generate_series(1, 5) m'
) AS (
month int,
week_1 int, week_2 int, week_3 int, week_4 int, week_5 int
) ORDER BY MONTH;
{% endcodeblock %}

The query produces:

```
 month | week_1 | week_2 | week_3 | week_4 | week_5 
-------+--------+--------+--------+--------+--------
     2 |      1 |        |      3 |      2 |       
     4 |      1 |        |        |        |       
     5 |      1 |        |        |        |       
    12 |        |        |        |      1 |       
(4 rows)
```

And this completes Day 2.