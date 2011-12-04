--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


--
-- Name: cube; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS cube WITH SCHEMA public;


--
-- Name: EXTENSION cube; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION cube IS 'data type for multidimensional cubes';


--
-- Name: dict_xsyn; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS dict_xsyn WITH SCHEMA public;


--
-- Name: EXTENSION dict_xsyn; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION dict_xsyn IS 'text search dictionary template for extended synonym processing';


--
-- Name: fuzzystrmatch; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS fuzzystrmatch WITH SCHEMA public;


--
-- Name: EXTENSION fuzzystrmatch; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION fuzzystrmatch IS 'determine similarities and distance between strings';


--
-- Name: pg_trgm; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS pg_trgm WITH SCHEMA public;


--
-- Name: EXTENSION pg_trgm; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION pg_trgm IS 'text similarity measurement and index searching based on trigrams';


--
-- Name: tablefunc; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS tablefunc WITH SCHEMA public;


--
-- Name: EXTENSION tablefunc; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION tablefunc IS 'functions that manipulate whole tables, including crosstab';


SET search_path = public, pg_catalog;

--
-- Name: add_event(text, timestamp without time zone, timestamp without time zone, text, character varying, character); Type: FUNCTION; Schema: public; Owner: fdumontmd
--

CREATE FUNCTION add_event(title text, starts timestamp without time zone, ends timestamp without time zone, venue text, postal character varying, country character) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
DECLARE
  did_insert boolean := false;
  found_count integer;
  the_venue_id integer;
BEGIN
  SELECT venue_id INTO the_venue_id
  FROM venues v
  WHERE v.postal_code = postal AND v.country_code = country AND v.name ILIKE venue
  LIMIT 1;

  IF the_venue_id IS NULL THEN
    INSERT INTO venues (name, postal_code, country_code)
    VALUES (venue, postal, country)
    RETURNING venue_id INTO the_venue_id;

    did_insert := true;
  END IF;

  RAISE NOTICE 'Venue found %', the_venue_id;

  INSERT INTO events (title, starts, ends, venue_id) VALUES (title, starts, ends, the_venue_id);

  RETURN did_insert;
END;
$$;


ALTER FUNCTION public.add_event(title text, starts timestamp without time zone, ends timestamp without time zone, venue text, postal character varying, country character) OWNER TO fdumontmd;

--
-- Name: log_event(); Type: FUNCTION; Schema: public; Owner: fdumontmd
--

CREATE FUNCTION log_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
BEGIN
  INSERT INTO logs (event_id, old_title, old_starts, old_ends)
  VALUES (OLD.event_id, OLD.title, OLD.starts, OLD.ends);

  RAISE NOTICE 'Someone just changed event #%', OLD.event_id;

  RETURN NEW;
END;
$$;


ALTER FUNCTION public.log_event() OWNER TO fdumontmd;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: cities; Type: TABLE; Schema: public; Owner: fdumontmd; Tablespace: 
--

CREATE TABLE cities (
    name text NOT NULL,
    postal_code character varying(9) NOT NULL,
    country_code character(2) NOT NULL,
    CONSTRAINT cities_postal_code_check CHECK (((postal_code)::text <> ''::text))
);


ALTER TABLE public.cities OWNER TO fdumontmd;

--
-- Name: countries; Type: TABLE; Schema: public; Owner: fdumontmd; Tablespace: 
--

CREATE TABLE countries (
    country_code character(2) NOT NULL,
    country_name text
);


ALTER TABLE public.countries OWNER TO fdumontmd;

--
-- Name: events; Type: TABLE; Schema: public; Owner: fdumontmd; Tablespace: 
--

CREATE TABLE events (
    event_id integer NOT NULL,
    title text,
    starts timestamp without time zone,
    ends timestamp without time zone,
    venue_id integer,
    colors text[]
);


ALTER TABLE public.events OWNER TO fdumontmd;

--
-- Name: events_event_id_seq; Type: SEQUENCE; Schema: public; Owner: fdumontmd
--

CREATE SEQUENCE events_event_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.events_event_id_seq OWNER TO fdumontmd;

--
-- Name: events_event_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: fdumontmd
--

ALTER SEQUENCE events_event_id_seq OWNED BY events.event_id;


--
-- Name: events_event_id_seq; Type: SEQUENCE SET; Schema: public; Owner: fdumontmd
--

SELECT pg_catalog.setval('events_event_id_seq', 18, true);


--
-- Name: holidays; Type: VIEW; Schema: public; Owner: fdumontmd
--

CREATE VIEW holidays AS
    SELECT events.event_id AS holiday_id, events.title AS name, events.starts AS date, events.colors FROM events WHERE ((events.title ~~ '%Day%'::text) AND (events.venue_id IS NULL));


ALTER TABLE public.holidays OWNER TO fdumontmd;

--
-- Name: logs; Type: TABLE; Schema: public; Owner: fdumontmd; Tablespace: 
--

CREATE TABLE logs (
    event_id integer,
    old_title character varying(255),
    old_starts timestamp without time zone,
    old_ends timestamp without time zone,
    logged_at timestamp without time zone DEFAULT now()
);


ALTER TABLE public.logs OWNER TO fdumontmd;

--
-- Name: observation; Type: TABLE; Schema: public; Owner: fdumontmd; Tablespace: 
--

CREATE TABLE observation (
    day timestamp without time zone NOT NULL,
    measure integer NOT NULL
);


ALTER TABLE public.observation OWNER TO fdumontmd;

--
-- Name: venues; Type: TABLE; Schema: public; Owner: fdumontmd; Tablespace: 
--

CREATE TABLE venues (
    venue_id integer NOT NULL,
    name character varying(255),
    street_address text,
    type character(7) DEFAULT 'public'::bpchar,
    postal_code character varying(9),
    country_code character(2),
    active boolean DEFAULT true,
    CONSTRAINT venues_type_check CHECK ((type = ANY (ARRAY['public'::bpchar, 'private'::bpchar])))
);


ALTER TABLE public.venues OWNER TO fdumontmd;

--
-- Name: venues_venue_id_seq; Type: SEQUENCE; Schema: public; Owner: fdumontmd
--

CREATE SEQUENCE venues_venue_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.venues_venue_id_seq OWNER TO fdumontmd;

--
-- Name: venues_venue_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: fdumontmd
--

ALTER SEQUENCE venues_venue_id_seq OWNED BY venues.venue_id;


--
-- Name: venues_venue_id_seq; Type: SEQUENCE SET; Schema: public; Owner: fdumontmd
--

SELECT pg_catalog.setval('venues_venue_id_seq', 6, true);


--
-- Name: event_id; Type: DEFAULT; Schema: public; Owner: fdumontmd
--

ALTER TABLE events ALTER COLUMN event_id SET DEFAULT nextval('events_event_id_seq'::regclass);


--
-- Name: venue_id; Type: DEFAULT; Schema: public; Owner: fdumontmd
--

ALTER TABLE venues ALTER COLUMN venue_id SET DEFAULT nextval('venues_venue_id_seq'::regclass);


--
-- Data for Name: cities; Type: TABLE DATA; Schema: public; Owner: fdumontmd
--

COPY cities (name, postal_code, country_code) FROM stdin;
Portland	97205	us
Shinjuku	160-0022	jp
\.


--
-- Data for Name: countries; Type: TABLE DATA; Schema: public; Owner: fdumontmd
--

COPY countries (country_code, country_name) FROM stdin;
us	United States
mx	Mexico
au	Australia
gb	United Kingdom
de	Germany
jp	Japan
\.


--
-- Data for Name: events; Type: TABLE DATA; Schema: public; Owner: fdumontmd
--

COPY events (event_id, title, starts, ends, venue_id, colors) FROM stdin;
1	My Book Signing	2012-02-15 17:30:00	2012-02-15 19:30:00	2	\N
2	April Fools Day	2012-04-01 00:00:00	2012-04-01 23:59:00	\N	\N
4	Your Favorite Band	2012-02-06 21:00:00	2012-02-06 23:00:00	1	\N
12	Steven King	2012-02-26 21:00:00	2012-02-26 23:00:00	2	\N
13	Dinner with Mom	2012-02-26 18:00:00	2012-02-26 20:30:00	4	\N
14	Valentine's Day	2012-02-14 00:00:00	2012-02-14 23:59:00	\N	\N
15	House Party	2012-05-03 23:00:00	2012-05-04 01:00:00	5	\N
3	Christmas Day	2012-12-25 00:00:00	2012-12-25 23:59:00	\N	{red,green}
18	Valentine's Day	2013-02-14 00:00:00	2013-02-14 23:59:00	\N	\N
\.


--
-- Data for Name: logs; Type: TABLE DATA; Schema: public; Owner: fdumontmd
--

COPY logs (event_id, old_title, old_starts, old_ends, logged_at) FROM stdin;
15	House Party	2012-05-03 23:00:00	2012-05-04 02:00:00	2011-12-04 12:35:45.915903
3	Christmas Day	2012-12-25 00:00:00	2012-12-25 23:59:00	2011-12-04 12:48:16.1513
\.


--
-- Data for Name: observation; Type: TABLE DATA; Schema: public; Owner: fdumontmd
--

COPY observation (day, measure) FROM stdin;
2011-12-02 00:00:00	55
2011-12-03 00:00:00	10
2011-12-04 00:00:00	47
2011-12-05 00:00:00	97
2011-12-06 00:00:00	58
2011-12-07 00:00:00	61
2011-12-08 00:00:00	76
2011-12-09 00:00:00	31
2011-12-10 00:00:00	89
2011-12-11 00:00:00	1
2011-12-12 00:00:00	43
2011-12-13 00:00:00	88
2011-12-14 00:00:00	14
2011-12-15 00:00:00	3
2011-12-16 00:00:00	34
2011-12-17 00:00:00	75
2011-12-18 00:00:00	9
2011-12-19 00:00:00	89
2011-12-20 00:00:00	50
2011-12-21 00:00:00	63
2011-12-22 00:00:00	76
2011-12-23 00:00:00	65
2011-12-24 00:00:00	95
2011-12-25 00:00:00	27
2011-12-26 00:00:00	26
2011-12-27 00:00:00	48
2011-12-28 00:00:00	4
2011-12-29 00:00:00	41
2011-12-30 00:00:00	98
2011-12-31 00:00:00	39
2012-01-01 00:00:00	76
2012-01-02 00:00:00	53
2012-01-03 00:00:00	48
2012-01-04 00:00:00	23
2012-01-05 00:00:00	49
2012-01-06 00:00:00	7
2012-01-07 00:00:00	84
2012-01-08 00:00:00	26
2012-01-09 00:00:00	38
2012-01-10 00:00:00	73
2012-01-11 00:00:00	27
2012-01-12 00:00:00	81
2012-01-13 00:00:00	61
2012-01-14 00:00:00	41
2012-01-15 00:00:00	84
2012-01-16 00:00:00	95
2012-01-17 00:00:00	17
2012-01-18 00:00:00	94
2012-01-19 00:00:00	84
2012-01-20 00:00:00	66
2012-01-21 00:00:00	57
2012-01-22 00:00:00	61
2012-01-23 00:00:00	31
2012-01-24 00:00:00	52
2012-01-25 00:00:00	87
2012-01-26 00:00:00	57
2012-01-27 00:00:00	99
2012-01-28 00:00:00	91
2012-01-29 00:00:00	98
2012-01-30 00:00:00	97
2012-01-31 00:00:00	30
2012-02-01 00:00:00	73
2012-02-02 00:00:00	50
2012-02-03 00:00:00	78
2012-02-04 00:00:00	96
2012-02-05 00:00:00	100
2012-02-06 00:00:00	85
2012-02-07 00:00:00	80
2012-02-08 00:00:00	25
2012-02-09 00:00:00	23
2012-02-10 00:00:00	53
2012-02-11 00:00:00	53
2012-02-12 00:00:00	4
2012-02-13 00:00:00	14
2012-02-14 00:00:00	94
2012-02-15 00:00:00	88
2012-02-16 00:00:00	9
2012-02-17 00:00:00	10
2012-02-18 00:00:00	82
2012-02-19 00:00:00	93
2012-02-20 00:00:00	76
2012-02-21 00:00:00	39
2012-02-22 00:00:00	54
2012-02-23 00:00:00	7
2012-02-24 00:00:00	90
2012-02-25 00:00:00	41
2012-02-26 00:00:00	64
2012-02-27 00:00:00	90
2012-02-28 00:00:00	33
2012-02-29 00:00:00	62
2012-03-01 00:00:00	87
2012-03-02 00:00:00	63
2012-03-03 00:00:00	35
2012-03-04 00:00:00	37
2012-03-05 00:00:00	41
2012-03-06 00:00:00	31
2012-03-07 00:00:00	37
2012-03-08 00:00:00	26
2012-03-09 00:00:00	11
2012-03-10 00:00:00	63
\.


--
-- Data for Name: venues; Type: TABLE DATA; Schema: public; Owner: fdumontmd
--

COPY venues (venue_id, name, street_address, type, postal_code, country_code, active) FROM stdin;
1	Crystal Ballroom	\N	public 	97205	us	t
2	Powell's Books	\N	public 	97205	us	t
5	Run's House	\N	public 	97205	us	t
4	My Place	\N	private	160-0022	jp	f
\.


--
-- Name: cities_pkey; Type: CONSTRAINT; Schema: public; Owner: fdumontmd; Tablespace: 
--

ALTER TABLE ONLY cities
    ADD CONSTRAINT cities_pkey PRIMARY KEY (country_code, postal_code);


--
-- Name: countries_country_name_key; Type: CONSTRAINT; Schema: public; Owner: fdumontmd; Tablespace: 
--

ALTER TABLE ONLY countries
    ADD CONSTRAINT countries_country_name_key UNIQUE (country_name);


--
-- Name: countries_pkey; Type: CONSTRAINT; Schema: public; Owner: fdumontmd; Tablespace: 
--

ALTER TABLE ONLY countries
    ADD CONSTRAINT countries_pkey PRIMARY KEY (country_code);


--
-- Name: events_pkey; Type: CONSTRAINT; Schema: public; Owner: fdumontmd; Tablespace: 
--

ALTER TABLE ONLY events
    ADD CONSTRAINT events_pkey PRIMARY KEY (event_id);


--
-- Name: observation_pkey; Type: CONSTRAINT; Schema: public; Owner: fdumontmd; Tablespace: 
--

ALTER TABLE ONLY observation
    ADD CONSTRAINT observation_pkey PRIMARY KEY (day);


--
-- Name: venues_pkey; Type: CONSTRAINT; Schema: public; Owner: fdumontmd; Tablespace: 
--

ALTER TABLE ONLY venues
    ADD CONSTRAINT venues_pkey PRIMARY KEY (venue_id);


--
-- Name: events_starts; Type: INDEX; Schema: public; Owner: fdumontmd; Tablespace: 
--

CREATE INDEX events_starts ON events USING btree (starts);


--
-- Name: delete_venue; Type: RULE; Schema: public; Owner: fdumontmd
--

CREATE RULE delete_venue AS ON DELETE TO venues DO INSTEAD UPDATE venues SET active = false WHERE (venues.venue_id = old.venue_id);


--
-- Name: insert_holidays; Type: RULE; Schema: public; Owner: fdumontmd
--

CREATE RULE insert_holidays AS ON INSERT TO holidays DO INSTEAD INSERT INTO events (title, starts, ends, colors) VALUES (new.name, new.date, (new.date + '23:59:00'::interval), new.colors);


--
-- Name: update_holidays; Type: RULE; Schema: public; Owner: fdumontmd
--

CREATE RULE update_holidays AS ON UPDATE TO holidays DO INSTEAD UPDATE events SET title = new.name, starts = new.date, colors = new.colors WHERE (events.title = old.name);


--
-- Name: log_events; Type: TRIGGER; Schema: public; Owner: fdumontmd
--

CREATE TRIGGER log_events AFTER UPDATE ON events FOR EACH ROW EXECUTE PROCEDURE log_event();


--
-- Name: cities_country_code_fkey; Type: FK CONSTRAINT; Schema: public; Owner: fdumontmd
--

ALTER TABLE ONLY cities
    ADD CONSTRAINT cities_country_code_fkey FOREIGN KEY (country_code) REFERENCES countries(country_code);


--
-- Name: events_venue_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: fdumontmd
--

ALTER TABLE ONLY events
    ADD CONSTRAINT events_venue_id_fkey FOREIGN KEY (venue_id) REFERENCES venues(venue_id);


--
-- Name: venues_country_code_fkey; Type: FK CONSTRAINT; Schema: public; Owner: fdumontmd
--

ALTER TABLE ONLY venues
    ADD CONSTRAINT venues_country_code_fkey FOREIGN KEY (country_code, postal_code) REFERENCES cities(country_code, postal_code) MATCH FULL;


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

