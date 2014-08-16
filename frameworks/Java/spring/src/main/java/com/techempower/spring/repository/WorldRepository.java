package com.techempower.spring.repository;

import com.techempower.spring.domain.World;
import org.springframework.data.jpa.repository.JpaRepository;

public interface WorldRepository extends JpaRepository<World, Integer> {

}
