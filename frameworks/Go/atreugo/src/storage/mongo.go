package storage

import (
	"context"
	"time"

	"atreugo/src/templates"

	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
	"go.mongodb.org/mongo-driver/mongo/readpref"
	"gopkg.in/mgo.v2/bson"
)

// Mongo struct
type Mongo struct {
	db       *mongo.Client
	database *mongo.Database
	// mongodb collections
	worlds   *mongo.Collection
	fortunes *mongo.Collection
}

// NewMongoDB creates new connection to postgres db with official mongo driver
func NewMongoDB(dbConnectionString string, maxConnectionsInPool int) (DB, error) {
	m := new(Mongo)

	if err := m.Connect(dbConnectionString, maxConnectionsInPool); err != nil {
		return nil, err
	}

	return m, nil
}

// Connect create connection and ping db
func (m *Mongo) Connect(dbConnectionString string, maxConnectionsInPool int) error {
	var err error

	opts := options.Client()
	opts.SetMaxPoolSize(uint64(maxConnectionsInPool))

	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()

	m.db, err = mongo.Connect(ctx, opts.ApplyURI(dbConnectionString))
	if err != nil {
		return err
	}

	ctx, cancel = context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()

	err = m.db.Ping(ctx, readpref.Primary())
	if err != nil {
		return err
	}

	m.database = m.db.Database("hello_world")
	m.worlds = m.database.Collection("world")
	m.fortunes = m.database.Collection("fortune")

	return nil
}

// Close connect to db
func (m *Mongo) Close() {
	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()

	if err := m.db.Disconnect(ctx); err != nil {
		panic(err)
	}
}

// GetOneRandomWorld return one random World struct
func (m Mongo) GetOneRandomWorld(w *World) error {
	var err error

	id := RandomWorldNum()
	filter := bson.M{"_id": id}

	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	if err = m.worlds.FindOne(ctx, filter).Decode(w); err != nil {
		return err
	}

	return err
}

// UpdateWorlds updates some number of worlds entries, passed as arg
func (m Mongo) UpdateWorlds(worlds Worlds) error {
	for _, w := range worlds {
		ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
		defer cancel()

		w.RandomNumber = int32(RandomWorldNum())

		if _, err := m.worlds.UpdateOne(
			ctx,
			bson.M{"_id": w.ID},
			bson.M{"$set": bson.M{"randomNumber": w.RandomNumber}},
		); err != nil {
			return err
		}
	}

	return nil
}

// GetFortunes selects all fortunes from table
func (m Mongo) GetFortunes() (templates.Fortunes, error) {
	fortunes := templates.AcquireFortunes()

	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	cur, err := m.fortunes.Find(ctx, bson.M{})
	if err != nil {
		return fortunes, err
	}
	defer cur.Close(ctx)

	fortune := templates.AcquireFortune()

	for cur.Next(context.Background()) {
		err = cur.Decode(fortune)
		if err != nil {
			return nil, err
		}

		fortunes = append(fortunes, *fortune)
	}

	templates.ReleaseFortune(fortune)

	return fortunes, cur.Err()
}
