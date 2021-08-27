package com.kartashov.fb.tuprolog.processors;

import com.kartashov.fb.tuprolog.Engine;
import com.kartashov.fb.tuprolog.Resource;
import it.unibo.tuprolog.core.Struct;
import it.unibo.tuprolog.core.Term;
import it.unibo.tuprolog.theory.Theory;

import java.util.Map;
import java.util.Objects;

public class WebResourceDirectiveProcessor extends DirectiveProcessor {

    @Override
    String functor() {
        return "web_resource";
    }

    @Override
    protected Theory apply(Struct target, Map<String, Term> properties, Theory theory, Engine engine) {

        var predicate = Objects.requireNonNull(target.getArgAt(0).asAtom()).getValue();
        var arity = Objects.requireNonNull(target.getArgAt(1).asInteger()).getValue().toInt();
        // @todo check arity for better match
        var contentTypeTerm = properties.get("content_type");
        var contentType = contentTypeTerm != null ? contentTypeTerm.toString() : "application/json";

        engine.addResource("/" + predicate, new Resource(predicate, new Resource.Writer(contentType)));
        return theory;
    }
}

