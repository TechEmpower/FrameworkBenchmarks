-- +migrate up
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    email VARCHAR(255) NOT NULL UNIQUE,
    name VARCHAR(255),
    created_at TIMESTAMP DEFAULT now()
);

CREATE INDEX idx_users_email ON users(email);

-- +migrate down
DROP TABLE users;
