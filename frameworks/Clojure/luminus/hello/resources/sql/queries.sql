--name: get-world
-- Query a World record from the database
SELECT * FROM world
WHERE id = :id

--name: get-all-fortunes
-- select all records from the fortune table
SELECT * FROM fortune

--name: update-world<!
-- update an existing world record
UPDATE world
SET randomNumber = :randomNumber
WHERE id = :id

--name: get-all-fortunes
-- query all fortune records
SELECT id, message FROM fortune


