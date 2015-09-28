package com.techempower.beyondj.repository;

import com.techempower.beyondj.domain.World;

import java.util.List;

public interface WorldRepository {

    World findOne(Integer id);

    long count();

    void save(World var1);

    List<World> findAll();

}
