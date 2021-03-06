-- The following command is if you want to begin all over with the database
-- DROP DATABASE haskitter_development;

CREATE DATABASE haskitter_development
  WITH OWNER = postgres
       ENCODING = 'UTF8'
       TABLESPACE = pg_default
       CONNECTION LIMIT = -1;

\connect haskitter_development;

CREATE TABLE users (
  id SERIAL NOT NULL,
  email VARCHAR(64) NOT NULL,
  name VARCHAR(70) NOT NULL,
  password VARCHAR NOT NULL,
  created_at TIMESTAMP without time zone NOT NULL default CURRENT_TIMESTAMP,
  CONSTRAINT users_pkey PRIMARY KEY (id)
);

CREATE UNIQUE INDEX index_users_on_email
  ON users
  USING btree
  (email);

CREATE TABLE posts (
  id SERIAL NOT NULL,
  message VARCHAR(140) NOT NULL,
  user_id INTEGER NOT NULL,
  created_at TIMESTAMP without time zone NOT NULL default CURRENT_TIMESTAMP,
  CONSTRAINT posts_pkey PRIMARY KEY (id),
  CONSTRAINT fk_posts_users FOREIGN KEY (user_id)
    REFERENCES users (id) MATCH SIMPLE
    ON UPDATE NO ACTION ON DELETE CASCADE
);

CREATE INDEX index_posts_on_user_id
  ON posts
  USING btree
  (user_id);

CREATE TABLE relationships (
  id SERIAL NOT NULL,
  follower_id INTEGER,
  followed_id INTEGER,
  created_at TIMESTAMP without time zone NOT NULL default CURRENT_TIMESTAMP,
  CONSTRAINT relationships_pkey PRIMARY KEY (id),
  CONSTRAINT fk_relationships_follower_id_users FOREIGN KEY (follower_id)
    REFERENCES users (id) MATCH SIMPLE
    ON UPDATE NO ACTION ON DELETE CASCADE,
  CONSTRAINT fk_relationships_followed_id_users FOREIGN KEY (followed_id)
    REFERENCES users (id) MATCH SIMPLE
    ON UPDATE NO ACTION ON DELETE CASCADE
);

CREATE INDEX index_relationships_on_followed_id
  ON relationships
  USING btree
  (followed_id);

CREATE INDEX index_relationships_on_follower_id
  ON relationships
  USING btree
  (follower_id);

CREATE UNIQUE INDEX index_relationships_on_follower_id_and_followed_id
  ON relationships
  USING btree
  (follower_id, followed_id);
