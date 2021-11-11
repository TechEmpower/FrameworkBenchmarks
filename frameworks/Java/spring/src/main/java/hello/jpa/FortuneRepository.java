package hello.jpa;

import hello.model.Fortune;
import org.springframework.context.annotation.Profile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Component;

@Component
@Profile("jpa")
public interface FortuneRepository extends JpaRepository<Fortune, Integer> {
}
