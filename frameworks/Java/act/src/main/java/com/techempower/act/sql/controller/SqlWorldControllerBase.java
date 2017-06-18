package com.techempower.act.sql.controller;

import act.db.ebean2.EbeanDao;
import act.db.ebean2.EbeanQuery;
import com.techempower.act.controller.WorldControllerBase;
import com.techempower.act.sql.domain.World;
import io.ebean.Transaction;

import javax.persistence.PersistenceException;
import java.util.ArrayList;
import java.util.List;

public abstract class SqlWorldControllerBase<
        MODEL_TYPE extends World,
        DAO_TYPE extends EbeanDao<Integer, MODEL_TYPE>>
        extends WorldControllerBase<MODEL_TYPE, EbeanQuery<MODEL_TYPE>, DAO_TYPE> {

    private boolean batchUpdate;

    public SqlWorldControllerBase(DAO_TYPE worldDao, boolean batch) {
        super(worldDao);
        this.batchUpdate = batch;
    }

    protected List<MODEL_TYPE> doUpdate(int q) {
        if (batchUpdate) {
            return doBatchUpdate(q);
        }
        List<MODEL_TYPE> retVal = new ArrayList<>();
        for (int i = 0; i < q; ++i) {
            Transaction tx = worldDao.ebean().beginTransaction();
            try {
                MODEL_TYPE world = findAndModifyOne();
                worldDao.save(world);
                tx.commit();
                retVal.add(world);
            } catch (PersistenceException e) {
                tx.rollback();
                throw e;
            }
        }
        return retVal;
    }

    private List<MODEL_TYPE> doBatchUpdate(int q) {
        List<MODEL_TYPE> retVal = new ArrayList<>();
        Transaction tx = worldDao.ebean().beginTransaction();
        try {
            for (int i = 0; i < q; ++i) {
                MODEL_TYPE world = findAndModifyOne();
                retVal.add(world);
            }
            worldDao.save(tx, retVal);
            tx.commit();
        } catch (PersistenceException e) {
            tx.rollback();
            throw e;
        }
        return retVal;
    }

}
