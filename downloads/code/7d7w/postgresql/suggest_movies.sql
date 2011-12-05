CREATE OR REPLACE FUNCTION suggest_movies(search text)
RETURNS SETOF movies AS $$
DECLARE
  found_name text;
  found_type char(1);
  tempmovie movies%rowtype;
  movie_query tsquery;
BEGIN
  movie_query := to_tsquery(replace(regexp_replace(trim(search), ' +' , ' ', 'g'), ' ', '&'));
  SELECT INTO found_name, found_type
              name, type
  FROM (
    SELECT a.name AS name, 'A' AS type, levenshtein(lower(search), lower(a.name)) AS dist
      FROM actors a WHERE search % a.name
    UNION
	SELECT a.name AS name, 'A' AS type, levenshtein(lower(search), lower(a.name)) AS dist
  	  FROM actors a WHERE metaphone(a.name, 6) = metaphone(search, 6)
	UNION
	SELECT m.title AS name, 'M' AS type, levenshtein(lower(search), lower(m.title)) AS dist
      FROM movies m WHERE to_tsvector('english', m.title) @@ movie_query
       ) t
  ORDER BY dist LIMIT 1;

  IF found_type = 'A' THEN
    FOR tempmovie IN SELECT m.* FROM movies m NATURAL JOIN movies_actors NATURAL JOIN actors
                     WHERE name = found_name LIMIT 5 LOOP
      RETURN NEXT tempmovie;
    END LOOP;
  ELSE
    FOR tempmovie IN SELECT m.* FROM movies m, 
                       (SELECT genre, title FROM movies WHERE title = found_name) s 
                       WHERE cube_enlarge(s.genre, 5, 18) @> m.genre AND s.title <> m.title
                       ORDER BY cube_distance(m.genre, s.genre) LIMIT 5 LOOP
      RETURN NEXT tempmovie;
    END LOOP;
  END IF;
END;
$$ LANGUAGE plpgsql;
