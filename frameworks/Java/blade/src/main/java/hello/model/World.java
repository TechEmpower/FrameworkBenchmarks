package hello.model;

import com.blade.jdbc.annotation.Table;
import com.blade.jdbc.core.ActiveRecord;

/**
 * World model
 *
 * @author biezhi
 * @date 2017/9/22
 */
@Table("world")
public class World extends ActiveRecord {

    private Integer id;
    private Integer randomNumber;

}
