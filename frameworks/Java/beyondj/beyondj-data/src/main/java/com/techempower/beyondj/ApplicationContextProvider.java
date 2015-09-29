package com.techempower.beyondj;

import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

/**
 * 
 * @author nickk
 * 
 */
public class ApplicationContextProvider implements ApplicationContextAware {

	private static ApplicationContext applicationContext = null;

	public static ApplicationContext getApplicationContext() {
		return applicationContext;
	}

	public void setApplicationContext(ApplicationContext _applicationContext) throws BeansException {
		applicationContext = _applicationContext;
	}
}
