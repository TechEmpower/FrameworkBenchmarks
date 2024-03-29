package hello.model;


import com.hellokaton.anima.Model;
import com.hellokaton.anima.annotation.Column;
import com.hellokaton.anima.annotation.Table;

/**
 * World model
 *
 * @author biezhi
 * @date 2018/10/17
 */
@Table(name = "world")
public class World extends Model {

    private Integer id;

    @Column(name = "randomNumber")
    private Integer randomNumber;

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public Integer getRandomNumber() {
        return randomNumber;
    }

    public void setRandomNumber(Integer randomNumber) {
        this.randomNumber = randomNumber;
    }
}
