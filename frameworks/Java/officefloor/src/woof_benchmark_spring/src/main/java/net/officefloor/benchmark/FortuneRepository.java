package net.officefloor.benchmark;

import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface FortuneRepository extends CrudRepository<Fortune, Integer> {
}
