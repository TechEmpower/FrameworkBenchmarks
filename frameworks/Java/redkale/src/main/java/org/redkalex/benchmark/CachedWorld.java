/*
 *
 */
package org.redkalex.benchmark;

import org.redkale.convert.json.JsonConvert;
import org.redkale.persistence.*;

/**
 *
 * @author zhangjx
 */
@Entity
@Cacheable
@Table(name = "world")
public final class CachedWorld {

    @Id
    private int id;

    private int randomNumber;

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public int getRandomNumber() {
        return randomNumber;
    }

    public void setRandomNumber(int randomNumber) {
        this.randomNumber = randomNumber;
    }

    @Override
    public String toString() {
        return JsonConvert.root().convertTo(this);
    }
}
