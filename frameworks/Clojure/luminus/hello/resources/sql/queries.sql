--:name get-all-fortunes :? :*
-- select all records from the fortune table
SELECT * FROM "Fortune"

-- :name update-world! :! :1
-- update an existing world record
UPDATE "World"
SET "randomnumber" = :randomNumber
WHERE id = :id

-- :name get-world :? :1
-- get world by id
SELECT * FROM "World"
WHERE id = :id
