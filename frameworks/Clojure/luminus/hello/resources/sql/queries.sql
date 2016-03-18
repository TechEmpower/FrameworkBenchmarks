-- :name get-worlds :? :*
SELECT * FROM "World"
WHERE id IN (:v*:ids)

--:name get-all-fortunes :? :*
-- select all records from the fortune table
SELECT * FROM "Fortune"

-- :name update-world! :! :1
-- update an existing world record
UPDATE "World"
SET "randomnumber" = :randomNumber
WHERE id = :id
