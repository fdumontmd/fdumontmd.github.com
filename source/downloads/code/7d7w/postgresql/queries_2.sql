-- change command a bit as order of column is different - make a note on the blog
INSERT INTO events (title, starts, ends, venue_id) VALUES ('Your Favorite Band', '2012-02-06 21:00', '2012-02-06 23:00', (
		SELECT venue_id FROM venues WHERE name = 'Crystal Ballroom' )
);

INSERT INTO countries (country_code, country_name) VALUES ('jp', 'Japan');
INSERT INTO cities (name, postal_code, country_code) VALUES ('Shinjuku', '160-0022', 'jp');
INSERT INTO venues (name, type, postal_code, country_code) VALUES ('My Place', 'private', '160-0022', 'jp');

INSERT INTO events (title, starts, ends, venue_id) VALUES
  ('Steven King', '2012-02-26 21:00:00', '2012-02-26 23:00:00', (SELECT venue_id FROM venues WHERE name = 'Powell''s Books')),
  ('Dinner with Mom', '2012-02-26 18:00:00', '2012-02-26 20:30:00', (SELECT venue_id FROM venues WHERE name = 'My Place')),
  ('Valentine''s Day', '2012-02-14 00:00:00', '2012-02-14 23:59:00', NULL);

CREATE RULE insert_holidays AS ON INSERT TO holidays DO INSTEAD INSERT INTO events (title, starts, ends, colors) values (NEW.name, NEW.date, NEW.date+interval '23 hour 59 minutes', NEW.colors);

1) add doc to create rule
CREATE RULE delete_venue AS ON DELETE TO venues DO INSTEAD UPDATE venues SET active = false WHERE venue_id = OLD.venue_id;

2) add doc to crosstab
SELECT * FROM crosstab(
'SELECT extract(year from starts) as year,
extract(month from starts) as month, count(*) FROM events
GROUP BY year, month',
  'SELECT m FROM generate_series(1, 12) m'
) AS (
year int,
jan int, feb int, mar int, apr int, may int, jun int, jul int, aug int, sep int, oct int, nov int, dec int
) ORDER BY YEAR;

3) 
SELECT * FROM crosstab(
'SELECT extract(month from starts) as month, 
 div(extract (day from starts)::int, 7) + 1as week, count(*) FROM events
GROUP BY month, week',
  'SELECT m FROM generate_series(1, 5) m'
) AS (
month int,
week_1 int, week_2 int, week_3 int, week_4 int, week_5 int
) ORDER BY MONTH;