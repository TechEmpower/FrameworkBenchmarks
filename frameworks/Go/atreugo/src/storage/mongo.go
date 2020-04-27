package storage

import (
	"context"

	"atreugo/src/templates"

	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
	"go.mongodb.org/mongo-driver/mongo/readpref"
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

	m.db, err = mongo.Connect(context.Background(), opts.ApplyURI(dbConnectionString))
	if err != nil {
		return err
	}

	err = m.db.Ping(context.Background(), readpref.Primary())
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
	if err := m.db.Disconnect(context.Background()); err != nil {
		panic(err)
	}
}

// GetOneRandomWorld return one random World struct
func (m *Mongo) GetOneRandomWorld(w *World) error {
	id := RandomWorldNum()
	filter := bson.M{"_id": id}

	return m.worlds.FindOne(context.Background(), filter).Decode(w)
}

// UpdateWorlds updates some number of worlds entries, passed as arg
func (m *Mongo) UpdateWorlds(worlds Worlds) error {
	var operations []mongo.WriteModel

	for _, w := range worlds {
		operation := mongo.NewUpdateOneModel()
		operation.SetFilter(bson.M{"_id": w.ID})
		operation.SetUpdate(bson.M{"$set": bson.M{"randomNumber": w.RandomNumber}})
		operations = append(operations, operation)
	}

	_, err := m.worlds.BulkWrite(context.Background(), operations)

	return err
}

// GetFortunes selects all fortunes from table
func (m *Mongo) GetFortunes() (templates.Fortunes, error) {
	cur, err := m.fortunes.Find(context.Background(), bson.M{})
	if err != nil {
		return nil, err
	}
	defer cur.Close(context.Background())

	fortunes := templates.AcquireFortunes()
	fortune := templates.AcquireFortune()

	for cur.Next(context.Background()) {
		cur.Decode(fortune)
		fortunes = append(fortunes, *fortune)
	}

	templates.ReleaseFortune(fortune)

	return fortunes, cur.Err()
}
