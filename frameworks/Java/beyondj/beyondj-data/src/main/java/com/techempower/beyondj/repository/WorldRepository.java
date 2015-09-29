package com.techempower.beyondj.repository;

import com.techempower.beyondj.domain.World;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

public interface WorldRepository {

    World findOne(Integer id);

    long count();

    @Transactional
    void save(World var1);

    List<World> findAll();

}
