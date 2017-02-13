package com.techempower.act.domain;

import act.util.SimpleBean;

/**
 * We need this interface because we want to support both MongoDB and MySQL in the same application
 */
public interface IWorld extends SimpleBean {
    Integer getId();
    Integer getRandomNumber();
}
