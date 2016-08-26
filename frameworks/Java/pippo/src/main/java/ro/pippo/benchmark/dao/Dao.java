package ro.pippo.benchmark.dao;

import java.util.List;
import ro.pippo.benchmark.model.Fortune;
import ro.pippo.benchmark.model.World;

public interface Dao {

  World getRandomWorld();

  void updateRandomWorlds(List<World> model);

  List<Fortune> getFortunes();
}
