package com.techempower.beyondj.repository;

import com.techempower.beyondj.domain.Fortune;
import org.springframework.data.repository.CrudRepository;

public interface FortuneRepository extends CrudRepository<Fortune, Integer> {
}
