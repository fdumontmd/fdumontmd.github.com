---
layout: post
title: "Seven Databases in Seven Weeks PostgreSQL Day 1"
date: 2011-12-03 12:22
comments: true
categories: [Books]
tags: [7databases7weeks, databases, postgresql]
series: "Seven Databases in Seven Weeks"
---
The first database in the series is a relational database, and what better choice than [PostgreSQL](http://www.postgresql.org)?
<!--more-->
Of course, [MySQL](http://www.mysql.com/) is more popular, but if we are going to talk about SQL and the relational model, it is better to pick a database that actually takes these concepts seriously.

The idea to start with a relational database is a good one. I feel that many who embrace NoSQL do so because they do not understand either SQL or the relational model. I was surprised to discover that some of my college educated colleagues did not know SQL at all. They were taught mostly about Java and object oriented design (I would ask for a refund). SQL databases have been developed, refined, and used in production for about 40 years. That's more than half the history of computers, they're likely to be part of the landscape for a few more decades, so ignoring them may be trendy but it is not wise.

About the PostgreSQL version: the book appears to have been long in the making, as it still refers the reader to the version 9.0, while [9.1](http://www.postgresql.org/docs/9.1/static/release-9-1-1.html) has been out for a few months.

Installing then extensions that are needed for this book is easier with 9.1. Just using [`CREATE EXTENSION`](http://www.postgresql.org/docs/current/static/sql-createextension.html):

{% codeblock Installing Extensions lang:sql %}
CREATE EXTENSION tablefunc;
CREATE EXTENSION dict_xsyn;
CREATE EXTENSION fuzzystrmatch;
CREATE EXTENSION pg_trgm;
CREATE EXTENSION cube;
{% endcodeblock %}

Removing them is done with the command [`DROP EXTENSION`](http://www.postgresql.org/docs/current/static/sql-dropextension.html).

### The events table

The code to create and fills the `events` table:

{% codeblock Creating and filling the events table lang:sql %}
CREATE TABLE events (
  event_id SERIAL PRIMARY KEY,
  title text,
  starts timestamp,
  ends timestamp,
  venue_id integer,
  FOREIGN KEY (venue_id)
    REFERENCES venues (venue_id)
);

INSERT INTO events (title, starts, ends, venue_id)
VALUES ('My Book Signing', '2012-02-15 17:30:00', '2012-02-15 19:30:00', 2),
       ('April Fools Day', '2012-04-01 00:00:00', '2012-04-01 23:59:00', NULL),
       ('Christmas Day',   '2012-12-25 00:00:00', '2012-12-25 23:59:00', NULL);
{% endcodeblock %}

### On indexes

I don't have a PostgreSQL 9.0 instance to check, but with 9.1 at least, `UNIQUE` constraints are implemented through a `btree` index, not a `hash` one. The difference is that a `btree` index is sorted and so supports range searches.

This can be checked by querying the console about the table:

```
book=# \d events
                                       Table "public.events"
  Column  |            Type             |                         Modifiers                         
----------+-----------------------------+-----------------------------------------------------------
 event_id | integer                     | not null default nextval('events_event_id_seq'::regclass)
 title    | text                        | 
 starts   | timestamp without time zone | 
 ends     | timestamp without time zone | 
 venue_id | integer                     | 
Indexes:
    "events_pkey" PRIMARY KEY, btree (event_id)
    "events_starts" btree (starts)
Foreign-key constraints:
    "events_venue_id_fkey" FOREIGN KEY (venue_id) REFERENCES venues(venue_id)
```

Exercises
---------

### The PostgreSQL FAQ

The FAQ is [here](http://wiki.postgresql.org/wiki/FAQ).

### The PostgreSQL Documentation

The documentation for version 9.1 is [here](http://www.postgresql.org/docs/9.1/static/index.html) (and [there](http://www.postgresql.org/docs/manuals/) for all the versions).

### About `MATCH FULL`

This one was already explained in the book, and confirmed by the [documentation](http://www.postgresql.org/docs/current/static/sql-createtable.html): when a foreign key is composed of more than one column, they must all match a row in the referenced table, or be all null.

### Selecting user table from `pg_class`

Interestingly, the first time I tried to solve this exercise, I used [`pg_tables`](http://www.postgresql.org/docs/current/static/view-pg-tables.html) by mistake (`pg_tables` has a `tableowner` column which makes it easy to identify user tables).

[`pg_class`](http://www.postgresql.org/docs/current/static/catalog-pg-class.html) stores the type of object in `relkind`: 'r' for tables. Restricting for just tables, and working on the table name:

```
book=#  select relname from pg_class where relkind = 'r';
         relname         
-------------------------
 pg_statistic
 pg_type
 pg_attribute
 pg_authid
 pg_proc
 pg_class
 pg_database
 pg_user_mapping
 pg_constraint
 pg_inherits
 pg_index
 pg_operator
 pg_opfamily
 pg_opclass
 pg_amop
 pg_amproc
 pg_language
 pg_largeobject_metadata
...
```

Ok, that's a bit long. Removing these `pg_` named tables:

```
book=# select relname from pg_class where relkind = 'r' and relname not like 'pg_%';
         relname         
-------------------------
 sql_implementation_info
 sql_languages
 sql_packages
 sql_parts
 sql_sizing
 sql_sizing_profiles
 sql_features
 countries
 events
 cities
 venues
(11 rows)
```

Interestingly, only the tables that are visible in the current database are listed (I have other databases, with more user created tables).

Still I need to remove the `sql_` named tables. Using PostgreSQL [regular expression operators](http://www.postgresql.org/docs/current/static/functions-matching.html#FUNCTIONS-POSIX-REGEXP):
```
book=# select relname from pg_class where relkind = 'r' and relname !~ '^(pg_|sql_)';
  relname  
-----------
 countries
 events
 cities
 venues
(4 rows)
```

This is one approach. Another would be to try to mimic the behaviour of the `\d` console command. The [`psql`](http://www.postgresql.org/docs/current/static/app-psql.html) option `-E` can be used to check what queries are used to implement specific console commands:

```
$ psql -E book
psql (9.1.1)
Type "help" for help.

book=# \d
********* QUERY **********
SELECT n.nspname as "Schema",
  c.relname as "Name",
  CASE c.relkind WHEN 'r' THEN 'table' WHEN 'v' THEN 'view' WHEN 'i' THEN 'index' WHEN 'S' THEN 'sequence' WHEN 's' THEN 'special' WHEN 'f' THEN 'foreign table' END as "Type",
  pg_catalog.pg_get_userbyid(c.relowner) as "Owner"
FROM pg_catalog.pg_class c
     LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
WHERE c.relkind IN ('r','v','S','f','')
      AND n.nspname <> 'pg_catalog'
      AND n.nspname <> 'information_schema'
      AND n.nspname !~ '^pg_toast'
  AND pg_catalog.pg_table_is_visible(c.oid)
ORDER BY 1,2;
**************************

                  List of relations
 Schema |        Name         |   Type   |   Owner   
--------+---------------------+----------+-----------
 public | cities              | table    | fdumontmd
 public | countries           | table    | fdumontmd
 public | events              | table    | fdumontmd
 public | events_event_id_seq | sequence | fdumontmd
 public | venues              | table    | fdumontmd
 public | venues_venue_id_seq | sequence | fdumontmd
(6 rows)
```

The minimal query that lists the user created tables in the current database seems to be:

```
book=# select c.relname from pg_class c join pg_namespace n on n.oid = c.relnamespace where c.relkind = 'r' and pg_catalog.pg_table_is_visible(c.oid) and n.nspname <> 'pg_catalog';
  relname  
-----------
 countries
 events
 cities
 venues
(4 rows)
```

But given that `pg_class` only lists the tables in the current database, and assuming you don't use `pg_` or `sql_` prefixes, the first query is clearly easier.

### Country name for the event 'My Book Signing'

Based on the content of the book so far, a possible solution uses `INNER JOIN`:

```
book=# select c.country_name from events e inner join venues v on e.venue_id = v.venue_id inner join countries c on v.country_code = c.country_code where title = 'My Book Signing';
 country_name  
---------------
 United States
(1 row)
```

But this is a bit verbose. The structure of the tables makes it possible to use [`NATURAL JOIN`](http://www.postgresql.org/docs/current/static/queries-table-expressions.html):

```
book=# select country_name from events natural join venues natural join countries where title = 'My Book Signing';
 country_name  
---------------
 United States
(1 row)
```

When using `NATURAL JOIN`, PostgreSQL will join on all the columns that are found in both tables (based on their names). For instance, both `events` and `venues` have a `venue_id` column (and no other column with the same name), so the join is on this column.

And other interesting difference is that `NATURAL JOIN` removes the duplicate columns:

```
book=# select * from events natural join venues natural join countries where title = 'My Book Signing';
 country_code | venue_id | event_id |      title      |       starts        |        ends         |     name      | street_address |  type   | postal_code | active | country_name  
--------------+----------+----------+-----------------+---------------------+---------------------+---------------+----------------+---------+-------------+--------+---------------
 us           |        2 |        1 | My Book Signing | 2012-02-15 17:30:00 | 2012-02-15 19:30:00 | Powel's Books |                | public  | 97205       | t      | United States
(1 row)
```

whereas `INNER JOIN` does not:
```
book=# select * from events e inner join venues v on e.venue_id = v.venue_id inner join countries c on v.country_code = c.country_code where title = 'My Book Signing';
 event_id |      title      |       starts        |        ends         | venue_id | venue_id |     name      | street_address |  type   | postal_code | country_code | active | country_code | country_name  
----------+-----------------+---------------------+---------------------+----------+----------+---------------+----------------+---------+-------------+--------------+--------+--------------+---------------
        1 | My Book Signing | 2012-02-15 17:30:00 | 2012-02-15 19:30:00 |        2 |        2 | Powel's Books |                | public  | 97205       | us           | t      | us           | United States
(1 row)
```

### Add a new column

{% codeblock Add a new column to `venues` lang:sql %}
ALTER TABLE venues ADD COLUMN active boolean DEFAULT TRUE;
{% endcodeblock %}

Checking that is is now there:
```
book=# \d venues
                                        Table "public.venues"
     Column     |          Type          |                         Modifiers                         
----------------+------------------------+-----------------------------------------------------------
 venue_id       | integer                | not null default nextval('venues_venue_id_seq'::regclass)
 name           | character varying(255) | 
 street_address | text                   | 
 type           | character(7)           | default 'public'::bpchar
 postal_code    | character varying(9)   | 
 country_code   | character(2)           | 
 active         | boolean                | default true
Indexes:
    "venues_pkey" PRIMARY KEY, btree (venue_id)
Check constraints:
    "venues_type_check" CHECK (type = ANY (ARRAY['public'::bpchar, 'private'::bpchar]))
Foreign-key constraints:
    "venues_country_code_fkey" FOREIGN KEY (country_code, postal_code) REFERENCES cities(country_code, postal_code) MATCH FULL
Referenced by:
    TABLE "events" CONSTRAINT "events_venue_id_fkey" FOREIGN KEY (venue_id) REFERENCES venues(venue_id)
```

or directly by `select`:
```
book=# select * from venues;
 venue_id |       name       | street_address |  type   | postal_code | country_code | active 
----------+------------------+----------------+---------+-------------+--------------+--------
        1 | Crystal Ballroom |                | public  | 97205       | us           | t
        2 | Powel's Books    |                | public  | 97205       | us           | t
(2 rows)
```

And this completes Day 1. Tomorrow, stored procedures, triggers, views, rules, and other goodies are on the menu.