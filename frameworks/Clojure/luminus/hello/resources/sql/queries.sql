-- :name get-world :? :*
-- Query a World record from the database
SELECT * FROM world
WHERE id = :id

--:name get-all-fortunes :? :*
-- select all records from the fortune table
SELECT * FROM fortune

-- :name update-world! :! :1
-- update an existing world record
UPDATE world
SET "randomNumber" = :randomnumber
WHERE id = :id
