package hello.repository;

import org.springframework.data.repository.ListCrudRepository;
import org.springframework.stereotype.Repository;

import hello.model.Fortune;

@Repository
interface FortuneDataJdbcRepository extends ListCrudRepository<Fortune, Integer> {
	
}