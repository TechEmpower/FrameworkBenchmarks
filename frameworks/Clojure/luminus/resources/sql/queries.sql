--:name get-all-fortunes :? :*
-- select all records from the fortune table
SELECT * FROM "fortune"

-- :name update-world! :! :1
-- update an existing world record
UPDATE "world"
SET "randomnumber" = :randomnumber
WHERE id = :id

-- :name get-world :? :1
-- get world by id
SELECT * FROM "world"
WHERE id = :id
