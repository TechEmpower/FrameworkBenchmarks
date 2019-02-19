DROP TABLE IF EXISTS posts;
DROP TABLE IF EXISTS tags;
DROP TABLE IF EXISTS users;

CREATE TABLE users (
    referrer_id     INT       REFERENCES users(rowid) ON DELETE SET NULL,
    activity_status BOOLEAN   NOT NULL
                              DEFAULT (1),
    role            INT       NOT NULL
                              DEFAULT (0),
    permissions     STRING    NOT NULL
                              DEFAULT ('{create_posts}'),
    favorite_numbers STRING   NOT NULL
                              DEFAULT ('{}'),
    name            STRING    NOT NULL,
    balance         REAL      NOT NULL
                              DEFAULT (0),
    meta            STRING    NOT NULL
                              DEFAULT ('{}'),
    created_at      DATETIME  NOT NULL
                              DEFAULT (STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW')),
    updated_at      DATETIME
);

CREATE TABLE tags (
    content TEXT NOT NULL
);

CREATE TABLE posts (
    author_id   INT       REFERENCES users (rowid)
                          NOT NULL,
    editor_id   INT       REFERENCES users (rowid),
    tag_ids     STRING,
    content     TEXT      NOT NULL,
    created_at  DATETIME  NOT NULL
                          DEFAULT (STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW')),
    updated_at  DATETIME
);

