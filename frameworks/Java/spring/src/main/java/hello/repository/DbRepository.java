package hello.repository;

import hello.model.Fortune;
import hello.model.World;

import java.util.List;

public interface DbRepository {
    World getWorld(int id);

    World findAndUpdateWorld(int id, int randomNumber);

    List<Fortune> fortunes();
}
