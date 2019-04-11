package com.example.helloworld.db.jdbi;

import com.example.helloworld.db.FortuneDAO;
import com.example.helloworld.db.model.Fortune;
import org.jdbi.v3.core.Jdbi;

import java.util.List;

public class FortuneRepository implements FortuneDAO {
    private final Jdbi jdbi;

    public FortuneRepository(Jdbi jdbi) {
        this.jdbi = jdbi;
    }

    @Override
    public List<Fortune> list() {
        return jdbi.withExtension(FortuneJDBIImpl.class, FortuneJDBIImpl::list);
    }
}
