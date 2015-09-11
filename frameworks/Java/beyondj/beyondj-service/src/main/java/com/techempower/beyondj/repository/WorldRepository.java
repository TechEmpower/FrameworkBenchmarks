package com.techempower.beyondj.repository;

import com.techempower.beyondj.domain.World;
import org.springframework.data.jpa.repository.JpaRepository;

public interface WorldRepository extends JpaRepository<World, Integer> {
}
