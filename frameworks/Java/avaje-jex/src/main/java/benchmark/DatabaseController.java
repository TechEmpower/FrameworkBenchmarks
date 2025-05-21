package benchmark;

import benchmark.model.Fortune;
import benchmark.model.FortuneTemplate;
import benchmark.model.World;
import benchmark.repository.DbService;
import io.avaje.http.api.Controller;
import io.avaje.http.api.Get;
import java.sql.SQLException;
import java.util.List;

@Controller
public class DatabaseController {

  private static final int MIN_QUERIES = 1;
  private static final int MAX_QUERIES = 500;

  private final DbService dbService;

  public DatabaseController(DbService dbService) {

    this.dbService = dbService;
  }

  @Get("/db")
  public World handleSingleDbQuery() throws SQLException {
    return dbService.getWorld(1).get(0);
  }

  @Get("/queries")
  public List<World> handleMultipleDbQueries(String queries) throws SQLException {
    int num = getBoundedRowNumber(queries);
    return dbService.getWorld(num);
  }

  @Get("/fortunes")
  public FortuneTemplate handleFortunes() throws SQLException {
    List<Fortune> fortuneList = dbService.getFortune();
    return new FortuneTemplate(fortuneList);
  }

  @Get("/updates")
  public List<World> handleUpdates(String queries) throws SQLException {
    int num = getBoundedRowNumber(queries);
    return dbService.updateWorld(num);
  }

  private static int getBoundedRowNumber(String number) {
    int num;
    try {
      num = Integer.parseInt(number);
    } catch (NumberFormatException e) {
      num = MIN_QUERIES;
    }
    return Math.max(MIN_QUERIES, Math.min(num, MAX_QUERIES));
  }
}
