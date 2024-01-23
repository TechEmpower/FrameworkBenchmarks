package hello.jpa;

import org.springframework.context.annotation.Profile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import hello.model.Fortune;

@Repository
@Profile("jpa")
public interface FortuneRepository extends JpaRepository<Fortune, Integer> {
}
