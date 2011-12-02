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
    venue_id integer
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

SELECT pg_catalog.setval('events_event_id_seq', 3, true);


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

SELECT pg_catalog.setval('venues_venue_id_seq', 2, true);


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
\.


--
-- Data for Name: events; Type: TABLE DATA; Schema: public; Owner: fdumontmd
--

COPY events (event_id, title, starts, ends, venue_id) FROM stdin;
1	My Book Signing	2012-02-15 17:30:00	2012-02-15 19:30:00	2
2	April Fools Day	2012-04-01 00:00:00	2012-04-01 23:59:00	\N
3	Christmas Day	2012-12-25 00:00:00	2012-12-25 23:59:00	\N
\.


--
-- Data for Name: venues; Type: TABLE DATA; Schema: public; Owner: fdumontmd
--

COPY venues (venue_id, name, street_address, type, postal_code, country_code) FROM stdin;
1	Crystal Ballroom	\N	public 	97205	us
2	Powel's Books	\N	public 	97205	us
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
-- Name: events_title_key; Type: CONSTRAINT; Schema: public; Owner: fdumontmd; Tablespace: 
--

ALTER TABLE ONLY events
    ADD CONSTRAINT events_title_key UNIQUE (title);


--
-- Name: venues_pkey; Type: CONSTRAINT; Schema: public; Owner: fdumontmd; Tablespace: 
--

ALTER TABLE ONLY venues
    ADD CONSTRAINT venues_pkey PRIMARY KEY (venue_id);


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

