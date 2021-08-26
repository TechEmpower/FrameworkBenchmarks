package com.kartashov.fb.tuprolog;

import com.kartashov.fb.tuprolog.processors.DirectiveProcessor;
import com.kartashov.fb.tuprolog.processors.WebResourceDirectiveProcessor;
import io.vertx.core.http.HttpServerRequest;
import it.unibo.tuprolog.core.Struct;
import it.unibo.tuprolog.core.Var;
import it.unibo.tuprolog.solve.MutableSolver;
import it.unibo.tuprolog.solve.Solver;
import it.unibo.tuprolog.theory.Theory;
import it.unibo.tuprolog.theory.parsing.ClausesReader;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Engine {

    private final Map<String, Resource> resources = new HashMap<>();
    private final MutableSolver solver = Solver.getClassic().mutableSolverWithDefaultBuiltins();

    public Engine(String... paths) {
        Theory theory = Theory.empty();
        ClausesReader reader = ClausesReader.getWithDefaultOperators();
        for (var path : paths) {
            try {
                var stream = new FileInputStream(Paths.get(path).toFile());
                var subTheory = reader.readTheory(stream);
                for (var processor : processors()) {
                    subTheory = processor.apply(subTheory, this);
                }
                theory = theory.plus(subTheory);
            } catch (FileNotFoundException e) {
                throw new RuntimeException("Cannot load file", e);
            }
        }
        solver.loadStaticKb(theory);
    }

    public void addResource(String path, Resource resource) {
        resources.put(path, resource);
    }

    public void resolve(HttpServerRequest request) {
        var resource = resources.get(request.path());
        if (resource == null) {
            request.response().setStatusCode(404).end();
            return;
        }
        var goalTerm = Struct.of(resource.predicate(), Var.of("Response"));
        var solution = solver.solveOnce(goalTerm);
        if (solution.isYes()) {
            var responseTerm = solution.getSubstitution().getByName("Response");
            if (responseTerm == null) {
                request.response().setStatusCode(503).end();
            } else {
                resource.writer().write(request.response(), responseTerm);
            }
        } else {
            request.response().setStatusCode(404).end();
        }
    }

    private List<DirectiveProcessor> processors() {
        return List.of(
                new WebResourceDirectiveProcessor()
        );
    }
}
