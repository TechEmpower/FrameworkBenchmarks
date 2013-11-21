package com.techempower.spring.repository;

import com.techempower.spring.domain.Fortune;
import org.springframework.data.jpa.repository.JpaRepository;

public interface FortuneRepository extends JpaRepository<Fortune, Integer> {

}
