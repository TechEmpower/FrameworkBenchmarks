package com.example.helloworld.db;

import com.example.helloworld.db.model.World;

public interface WorldDAO {
    World findById(int id);
    World findAndModify(int id, int newRandomNumber);
    World[] updatesQueries(int queries);
}
