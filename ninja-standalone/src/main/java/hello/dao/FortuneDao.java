package hello.dao;

import hello.model.Fortune;

import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.Query;

import com.google.inject.Inject;
import com.google.inject.Provider;
import com.google.inject.Singleton;
import com.google.inject.persist.Transactional;

@Singleton
public class FortuneDao {

    @Inject
    Provider<EntityManager> entitiyManagerProvider;

    @Transactional
    public List<Fortune> getAll() {
	EntityManager entityManager = entitiyManagerProvider.get();

	Query q = entityManager.createQuery("SELECT x FROM Fortune x");
	List<Fortune> fortunes = q.getResultList();

	return fortunes;
    }

}
