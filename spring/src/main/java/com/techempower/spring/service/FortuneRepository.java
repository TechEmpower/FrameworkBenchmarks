package com.techempower.spring.service;

import com.techempower.spring.domain.Fortune;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

public interface FortuneRepository extends CrudRepository<Fortune, Integer> {

	@Override
	public List<Fortune> findAll();

}
