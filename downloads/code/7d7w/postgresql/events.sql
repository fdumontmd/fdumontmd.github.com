CREATE TABLE events (
  event_id SERIAL PRIMARY KEY,
  title text UNIQUE,
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
