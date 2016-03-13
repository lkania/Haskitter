-- The following command is if you want to begin all over with the database
-- DROP DATABASE haskitter_development;

CREATE DATABASE haskitter_development
  WITH OWNER = postgres
       ENCODING = 'UTF8'
       TABLESPACE = pg_default
       LC_COLLATE = 'en_US.UTF-8'
       LC_CTYPE = 'en_US.UTF-8'
       CONNECTION LIMIT = -1;

\connect haskitter_development;

CREATE TABLE posts (
  id serial NOT NULL,
	message VARCHAR(140) NOT NULL,
  user_id INTEGER NOT NULL,
  created_at TIMESTAMP without time zone NOT NULL,
  CONSTRAINT posts_pkey PRIMARY KEY (id)
);

CREATE INDEX index_posts_on_user_id
  ON posts
  USING btree
  (user_id);
