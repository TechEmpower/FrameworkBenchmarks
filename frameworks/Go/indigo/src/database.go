package main

import (
	"context"

	"github.com/jackc/pgx/v5"
	"github.com/jackc/pgx/v5/pgxpool"

	"indigo/app/models"
)

type (
	DB struct {
		pool *pgxpool.Pool
	}
)

func NewDB(pool *pgxpool.Pool) *DB {
	return &DB{
		pool: pool,
	}
}

func (DB *DB) GetFortunes(ctx context.Context) ([]models.Fortune, error) {
	query := `SELECT id, message FROM Fortune`

	rows, err := DB.pool.Query(ctx, query)
	if err != nil {
		return nil, err
	}

	fortunes, fortune := []models.Fortune(nil), models.Fortune{}

	for rows.Next() {
		err = rows.Scan(&fortune.ID, &fortune.Message)
		if err != nil {
			return nil, err
		}

		fortunes = append(fortunes, fortune)
	}

	err = rows.Err()
	if err != nil {
		return nil, err
	}

	return fortunes, nil
}

func (DB *DB) FillWorldByID(ctx context.Context, world *models.World) error {
	query := `SELECT randomNumber FROM World WHERE id = $1 LIMIT 1`

	return DB.pool.QueryRow(
		context.Background(),
		query,
		world.ID,
	).Scan(&world.RandomNumber)
}

func (DB *DB) FillWorlds(ctx context.Context, worlds []models.World) error {
	query := `SELECT id, randomNumber FROM World`

	rows, err := DB.pool.Query(ctx, query)
	if err != nil {
		return err
	}

	i, world := 0, (*models.World)(nil)

	for rows.Next() && i < len(worlds) {
		world = &worlds[i]

		err = rows.Scan(&world.ID, &world.RandomNumber)
		if err != nil {
			return err
		}

		i += 1
	}

	return rows.Err()
}

func (DB *DB) FillWorldsByID(ctx context.Context, worlds []models.World) error {
	connection, err := DB.pool.Acquire(ctx)
	if err != nil {
		return err
	}
	defer connection.Release()

	query := `SELECT randomNumber FROM World WHERE id = $1 LIMIT 1`

	i, world, err := 0, (*models.World)(nil), error(nil)

	for i = range worlds {
		world = &worlds[i]

		err = connection.QueryRow(
			context.Background(),
			query,
			world.ID,
		).Scan(&world.RandomNumber)
		if err != nil {
			return err
		}
	}

	return nil
}

func (DB *DB) UpdateWorlds(ctx context.Context, worlds models.Worlds) error {
	query := `UPDATE World SET randomNumber = $1 WHERE id = $2`
	batch := new(pgx.Batch)

	i, world := 0, (*models.World)(nil)

	for i = range worlds {
		world = &worlds[i]
		batch.Queue(query, &world.RandomNumber, &world.ID)
	}

	return DB.pool.SendBatch(context.Background(), batch).Close()
}
