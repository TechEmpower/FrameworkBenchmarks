package hello.jpa;

import hello.model.World;
import org.springframework.context.annotation.Profile;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Component;

@Component
@Profile("jpa")
public interface WorldRepository extends JpaRepository<World, Integer> {
}
