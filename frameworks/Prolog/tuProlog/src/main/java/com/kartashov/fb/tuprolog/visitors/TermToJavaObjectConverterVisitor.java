package com.kartashov.fb.tuprolog.visitors;

import it.unibo.tuprolog.core.Integer;
import it.unibo.tuprolog.core.*;
import it.unibo.tuprolog.core.visitors.DefaultTermVisitor;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

public class TermToJavaObjectConverterVisitor extends DefaultTermVisitor<Object> {

    @Override
    public Object defaultValue(@NotNull Term term) {
        return null;
    }

    @Override
    public Object visitConstant(@NotNull Constant term) {
        return term.getValue();
    }

    @Override
    public Object visitInteger(@NotNull Integer term) {
        return term.getValue().toLong();
    }

    @Override
    public Object visitReal(@NotNull Real term) {
        return term.getValue().toDouble();
    }

    @Override
    public Object visitStruct(@NotNull Struct term) {
        return Map.of(term.getFunctor(), term.getArgAt(0).accept(this));
    }

    @Override
    public Object visitList(@NotNull List term) {
        if (term.toList().stream().allMatch(Term::isStruct)) {
            var map = new LinkedHashMap<String, Object>();
            term.toList().stream()
                    .map(Term::asStruct)
                    .filter(Objects::nonNull)
                    .forEach(s -> map.put(s.getFunctor(), s.getArgAt(0).accept(this)));
            return map;
        } else {
            return term.toList().stream().map(t -> t.accept(this)).collect(Collectors.toCollection(ArrayList::new));
        }
    }

    @Override
    public Object visitAtom(@NotNull Atom term) {
        return term.getValue();
    }
}

