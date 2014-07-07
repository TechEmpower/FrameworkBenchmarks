package fi.markoa.tfb.servlet3;

import com.google.common.util.concurrent.ListenableFuture;

import java.util.List;
import java.util.Properties;

public interface MessageDAO {
  void init();
  ListenableFuture<World> read(int id);
  ListenableFuture<List<World>> read(List<Integer> ids);
  void destroy();
}
