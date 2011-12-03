select tablename from pg_tables where tableowner = 'fdumontmd';
select country_name from events natural join venues natural join countries where title = 'My Book Signing';
select c.country_name from events e inner join venues v on e.venue_id = v.venue_id inner join countries c on v.country_code = c.country_code where title = 'My Book Signing';

ALTER TABLE venues ADD COLUMN active boolean DEFAULT TRUE;
