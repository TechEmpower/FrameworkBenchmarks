-- +micrate Up
CREATE TABLE fortunes (
  id BIGSERIAL PRIMARY KEY,
  message TEXT,
  created_at TIMESTAMP,
  updated_at TIMESTAMP
);

-- +micrate Down
DROP TABLE IF EXISTS fortunes;
