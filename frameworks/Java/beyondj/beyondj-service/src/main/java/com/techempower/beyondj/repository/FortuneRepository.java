package com.techempower.beyondj.repository;

import com.techempower.beyondj.domain.Fortune;
import org.springframework.data.jpa.repository.JpaRepository;

public interface FortuneRepository extends JpaRepository<Fortune, Integer> {
}
