package com.techempower.beyondj.repository;

import org.hibernate.*;
import org.springframework.dao.DataAccessException;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;

import java.io.Serializable;

@Transactional
public class Repository {

	private SessionFactory sessionFactory;
	private TransactionTemplate transactionTemplate;

	public boolean contains(Object obj) {
		return getSession().contains(obj);
	}

	@SuppressWarnings("rawtypes")
	public Criteria createCriteria(Class obj) {
		return getSession().createCriteria(obj);
	}

	public Criteria createCriteria(String obj) {
		return getSession().createCriteria(obj);
	}

	public void attach(Object object) throws DataAccessException {
		try {
			getSession().lock(object, LockMode.NONE);
		} catch (HibernateException ex) {
		}
	}

	public TransactionTemplate getTransactionTemplate() {
		return transactionTemplate;
	}

	public void setTransactionTemplate(TransactionTemplate transactionTemplate) {
		this.transactionTemplate = transactionTemplate;
	}

	@SuppressWarnings("rawtypes")
	public Criteria createCriteria(Class obj, String arg1) {
		return getSession().createCriteria(obj, arg1);
	}

	public Criteria createCriteria(String obj, String arg1) {
		return getSession().createCriteria(obj, arg1);
	}

	public Query createFilter(Object obj, String arg1) {
		return getSession().createFilter(obj, arg1);
	}

	public Query createQuery(String obj) {
		return getSession().createQuery(obj);
	}

	public SQLQuery createSQLQuery(String obj) {
		return getSession().createSQLQuery(obj);
	}


	@SuppressWarnings("rawtypes")
	public Object get(Class obj, Serializable arg1) {
		return (Object) getSession().get(obj, arg1);
	}

	public Object get(String obj, Serializable arg1) {
		return (Object) getSession().get(obj, arg1);
	}

	@SuppressWarnings({ "rawtypes" })
	public Object get(Class obj, Serializable arg1, LockMode arg2) {
		return (Object) getSession().get(obj, arg1, arg2);
	}

	public Object get(String obj, Serializable arg1, LockMode arg2) {
		return (Object) getSession().get(obj, arg1, arg2);
	}

	public CacheMode getCacheMode() {
		return getSession().getCacheMode();
	}

	public String getEntityName(Object obj) {
		return getSession().getEntityName(obj);
	}

	public FlushMode getFlushMode() {
		return getSession().getFlushMode();
	}

	public Serializable getIdentifier(Object obj) {
		return getSession().getIdentifier(obj);
	}

	public Query getNamedQuery(String obj) {
		return getSession().getNamedQuery(obj);
	}

	@SuppressWarnings("rawtypes")
	public Object load(Class obj, Serializable arg1) {
		return (Object) getSession().load(obj, arg1);
	}

	public Object load(String obj, Serializable arg1) {
		return (Object) getSession().load(obj, arg1);
	}

	public Object load(Object obj, Serializable arg1) {
		getSession().load(obj, arg1);
		return obj;
	}

	@SuppressWarnings("rawtypes")
	public Object load(Class obj, Serializable arg1, LockMode arg2) {
		return (Object) getSession().load(obj, arg1, arg2);
	}

	public Object load(String obj, Serializable arg1, LockMode arg2) {
		return (Object) getSession().load(obj, arg1, arg2);
	}

	public Object merge(Object obj) {
		getSession().merge(obj);
		return obj;
	}

	public Object merge(String obj, Object arg1) {
		getSession().merge(obj, arg1);
		return arg1;
	}

	public Object persist(Object obj) {
		getSession().persist(obj);
		return obj;
	}

	public Object persist(String obj, Object arg1) {
		getSession().persist(obj, arg1);
		return arg1;
	}

	public Object refresh(Object obj) {
		getSession().refresh(obj);
		return obj;
	}

	public Object refresh(Object obj, LockMode arg1) {
		getSession().refresh(obj, arg1);
		return obj;
	}

	protected Session getSession() {
		return sessionFactory.getCurrentSession();
	}

	protected Session getCurrentSession() {
		return sessionFactory.getCurrentSession();
	}

	public SessionFactory getSessionFactory() {
		return sessionFactory;
	}

	public void setSessionFactory(SessionFactory sessionFactory) {
		this.sessionFactory = sessionFactory;
	}

	protected static final String ID = "id";
}
