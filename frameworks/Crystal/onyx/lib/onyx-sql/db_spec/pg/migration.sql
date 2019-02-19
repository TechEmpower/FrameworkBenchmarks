DROP TABLE IF EXISTS posts;
DROP TABLE IF EXISTS tags;
DROP TABLE IF EXISTS users;
DROP TYPE IF EXISTS users_role;
DROP TYPE IF EXISTS users_permissions;
DROP EXTENSION IF EXISTS pgcrypto;
CREATE EXTENSION pgcrypto;

CREATE TYPE users_role AS ENUM ('writer', 'moderator', 'admin');
CREATE TYPE users_permissions AS ENUM ('create_posts', 'edit_posts');

CREATE TABLE users(
  uuid            UUID PRIMARY KEY              DEFAULT gen_random_uuid(),
  referrer_uuid   UUID                          REFERENCES users (uuid) ON DELETE SET NULL,
  activity_status BOOL                NOT NULL  DEFAULT true,
  role            users_role          NOT NULL  DEFAULT 'writer',
  permissions     users_permissions[] NOT NULL  DEFAULT '{create_posts}',
  favorite_numbers INT[]              NOT NULL  DEFAULT '{}',
  name            VARCHAR(100)        NOT NULL,
  balance         REAL                NOT NULL  DEFAULT 0,
  meta            JSON                NOT NULL  DEFAULT '{}',
  created_at      TIMESTAMPTZ         NOT NULL  DEFAULT NOW(),
  updated_at      TIMESTAMPTZ
);

CREATE TABLE tags(
  id      SERIAL  PRIMARY KEY,
  content TEXT    NOT NULL
);

CREATE TABLE posts(
  id          SERIAL PRIMARY KEY,
  author_uuid UUID        NOT NULL  REFERENCES users (uuid),
  editor_uuid UUID                  REFERENCES users (uuid),
  tag_ids     INT[],
  content     TEXT        NOT NULL,
  created_at  TIMESTAMPTZ NOT NULL  DEFAULT NOW(),
  updated_at  TIMESTAMPTZ
);
