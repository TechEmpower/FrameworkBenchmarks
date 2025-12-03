package com.kfyty.benchmark.example.repository;

import com.kfyty.benchmark.example.model.Fortune;
import com.kfyty.benchmark.example.model.World;

import java.util.List;

public interface DbRepository {

	World getWorld(int id);

	void updateWorlds(List<World> worlds);

	List<Fortune> fortunes();
}
