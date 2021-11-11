package models;

import java.util.List;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.criteria.CriteriaQuery;

import play.db.jpa.JPAApi;

@Entity
public class Fortune {

    @Id
    public Long id = 0L;

    public String message;

    public Fortune() {
    }

    public Fortune(final String message) {
        this.message = message;
    }

    public static List<Fortune> findAll(final JPAApi jpa) {
        return jpa.withTransaction("default", true, em -> {
            final CriteriaQuery<Fortune> criteria = em.getCriteriaBuilder().createQuery(Fortune.class);
            criteria.select(criteria.from(Fortune.class));
            return em.createQuery(criteria).getResultList();
        });
    }
}
