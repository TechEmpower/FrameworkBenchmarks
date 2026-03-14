package hello.repository;

import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

import hello.model.World;

@Repository
interface WorldDataJdbcRepository extends CrudRepository<World, Integer> {
	
}