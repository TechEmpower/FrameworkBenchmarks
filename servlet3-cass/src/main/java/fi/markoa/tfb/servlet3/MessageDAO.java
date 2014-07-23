package fi.markoa.tfb.servlet3;

import com.google.common.util.concurrent.ListenableFuture;
import com.google.common.util.concurrent.ListeningExecutorService;

import java.util.List;

public interface MessageDAO {
  void init(ListeningExecutorService executorService);
  ListenableFuture<World> read(int id);
  ListenableFuture<List<World>> read(List<Integer> ids);
  ListenableFuture<Void> update(List<World> worlds);
  void destroy();
}
