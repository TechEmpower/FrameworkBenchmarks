package com.techempower.act.domain;

import act.util.SimpleBean;
import org.jetbrains.annotations.NotNull;

/**
 * We need this interface because we want to support both MongoDB and MySQL in the same application
 */
public interface IFortune extends SimpleBean, Comparable<IFortune> {
    Integer getId();
    String getMessage();

    @Override
    default int compareTo(@NotNull IFortune o) {
        return getMessage().compareTo(o.getMessage());
    }
}
