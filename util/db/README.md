# Database Documentation

*pg_haskitter.sql* file is for the database schema.

## DB Tables

### Users

|      id     |         email         |          name         |  password_digest  |          created_at         |
|:-----------:|:---------------------:|:---------------------:|:-----------------:|:---------------------------:|
| [PK] serial | character varying(64) | character varying(70) | character varying | timestamp without time zone |

- It has a UNIQUE INDEX on the `email` column.

### Posts

|      id     | message                | user_id |           created_at        |
|:-----------:|------------------------|---------|:---------------------------:|
| [PK] serial | character varying(140) | integer | timestamp without time zone |

- It has a FOREIGN KEY on the column `user_id` that references the `id` of Users table.
- It has an INDEX on the `user_id` column.
