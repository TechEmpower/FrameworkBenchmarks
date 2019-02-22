	for i := 0; i < n; i++ {
		query := bson.M{"_id": getRandomNumber()}
		if err := worlds.Find(query).One(&world[i]); err != nil {
			log.Fatalf("Error finding world with id: %s", err.Error())
		}
		world[i].RandomNumber = getRandomNumber()
		update := bson.M{"$set": bson.M{"randomNumber": world[i].RandomNumber}}
		if err := worlds.Update(query, update); err != nil {
			log.Fatalf("Error updating world with id: %s", err.Error())
		}
	}