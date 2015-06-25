package models;

import play.db.jpa.JPA;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.criteria.CriteriaQuery;
import java.util.List;

@Entity
public class Fortune {

    @Id
    public Long id = 0L;

    public String message;

    public Fortune() {
    }

    public Fortune(String message) {
        this.message = message;
    }

    public static List<Fortune> findAll() throws Throwable {
        return JPA.withTransaction("default", true, () -> {
            CriteriaQuery<Fortune> criteria = JPA.em().getCriteriaBuilder().createQuery(Fortune.class);
            criteria.select(criteria.from(Fortune.class));
            return JPA.em().createQuery(criteria).getResultList();
        });
    }
}
