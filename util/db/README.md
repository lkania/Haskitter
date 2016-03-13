# Database Documentation

*pg_haskitter.sql* file is for the database schema.

## DB Tables

### Posts

|      id     | message                | user_id |           created_at        |
|:-----------:|------------------------|---------|:---------------------------:|
| [PK] serial | character varying(140) | integer | timestamp without time zone |

It also has an index on the `user_id` column.
