-- :name get-worlds :? :*
SELECT * FROM world
WHERE id IN (:v*:ids)

--:name get-all-fortunes :? :*
-- select all records from the fortune table
SELECT * FROM fortune

-- :name update-world! :! :1
-- update an existing world record
UPDATE world
SET "randomNumber" = :randomNumber
WHERE id = :id
