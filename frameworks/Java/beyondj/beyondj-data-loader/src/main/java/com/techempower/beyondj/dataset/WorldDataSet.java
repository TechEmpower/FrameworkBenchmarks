package com.techempower.beyondj.dataset;


import com.techempower.beyondj.domain.World;
import org.apache.commons.io.IOUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.data.jpa.repository.support.SimpleJpaRepository;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.persistence.EntityManagerFactory;
import java.util.List;

@Component
public class WorldDataSet {

    public static final String IMPORT_SQL = "import.sql";
    @Autowired
    private EntityManagerFactory entityManagerFactory;
    @Autowired
    private JdbcTemplate jdbcTemplate;

    public WorldDataSet() {
    }

    @PostConstruct
    public void create() throws Exception {
        if (getRepository().count() == 0) {
            Resource resource = new ClassPathResource(IMPORT_SQL);
            List<String> lines = IOUtils.readLines(resource.getInputStream());
            String[] array = new String[lines.size()];
            jdbcTemplate.batchUpdate(lines.toArray(array));
        }
    }

    private SimpleJpaRepository getRepository() {
        synchronized (WorldDataSet.class) {
            return new SimpleJpaRepository<>(
                    World.class, entityManagerFactory.createEntityManager());
        }
    }
}
