package com.techempower.spring.service;

import com.techempower.spring.domain.World;
import org.springframework.data.repository.CrudRepository;

public interface WorldRepository extends CrudRepository<World, Integer> {
}
