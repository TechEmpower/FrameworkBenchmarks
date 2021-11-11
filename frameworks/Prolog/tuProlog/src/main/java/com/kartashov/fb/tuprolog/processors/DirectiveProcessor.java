package com.kartashov.fb.tuprolog.processors;

import com.kartashov.fb.tuprolog.Engine;
import it.unibo.tuprolog.core.Struct;
import it.unibo.tuprolog.core.Term;
import it.unibo.tuprolog.theory.Theory;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public abstract class DirectiveProcessor {

    abstract String functor();

    protected Theory apply(Struct target, Map<String, Term> properties, Theory theory, Engine engine) {
        return theory;
    }

    final public Theory apply(Theory theory, Engine engine) {
        for (var directive: theory.getDirectives()) {
            var body = directive.getBody().asStruct();
            if (body == null || !body.getFunctor().equals(functor())) {
                continue;
            }
            Struct target = null;
            var properties = new HashMap<String, Term>();
            var i = 0;
            for (var arg: body.getArgs()) {
                Struct argStruct = Objects.requireNonNull(arg.asStruct());
                if (i++ == 0) {
                    target = argStruct;
                } else {
                    properties.put(argStruct.getFunctor(), argStruct.getArgAt(0));
                }
            }
            theory = apply(target, properties, theory, engine);
        }
        return theory;
    }
}

