package com.techempower.beyondj.repository;

import com.techempower.beyondj.domain.Fortune;
import com.techempower.beyondj.domain.World;

import java.util.List;

public interface FortuneRepository {

    Fortune findOne(Integer id);

    long count();

    void save(Fortune var1);

    List<Fortune> findAll();

}
