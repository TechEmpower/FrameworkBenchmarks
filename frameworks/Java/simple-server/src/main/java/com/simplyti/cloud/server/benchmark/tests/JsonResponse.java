package com.simplyti.cloud.server.benchmark.tests;

import com.dslplatform.json.CompiledJson;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.experimental.Accessors;

@AllArgsConstructor(onConstructor=@__(@CompiledJson))
@Getter
@Accessors(fluent=true)
public class JsonResponse {

	private final String message;

}
