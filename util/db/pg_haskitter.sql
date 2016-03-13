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

CREATE TABLE users (
  id serial NOT NULL,
  email VARCHAR(64) NOT NULL,
  name VARCHAR(70) NOT NULL,
  password_digest VARCHAR NOT NULL,
  created_at TIMESTAMP without time zone NOT NULL,
  CONSTRAINT users_pkey PRIMARY KEY (id)
);

CREATE UNIQUE INDEX index_users_on_email
  ON users
  USING btree
  (email);

CREATE TABLE posts (
  id serial NOT NULL,
  message VARCHAR(140) NOT NULL,
  user_id INTEGER NOT NULL,
  created_at TIMESTAMP without time zone NOT NULL,
  CONSTRAINT posts_pkey PRIMARY KEY (id),
  CONSTRAINT fk_posts_users FOREIGN KEY (user_id)
    REFERENCES users (id) MATCH SIMPLE
    ON UPDATE NO ACTION ON DELETE NO ACTION
);

CREATE INDEX index_posts_on_user_id
  ON posts
  USING btree
  (user_id);
