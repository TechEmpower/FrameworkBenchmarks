package com.techempower.beyondj.dataset;


import com.techempower.beyondj.repository.WorldRepository;
import org.apache.commons.io.IOUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.jdbc.core.JdbcTemplate;

import javax.annotation.PostConstruct;
import java.util.List;

public class WorldDataSet {

    @Autowired
    private WorldRepository worldRepository;
    @Autowired
    private JdbcTemplate jdbcTemplate;

    public WorldDataSet() {
    }

    @PostConstruct
    public void create() throws Exception {
        if (worldRepository.count() == 0) {
            Resource resource = new ClassPathResource("import.sql");
            List<String> lines = IOUtils.readLines(resource.getInputStream());
            String[] array = new String[lines.size()];
            jdbcTemplate.batchUpdate(lines.toArray(array));
        }
    }
}
