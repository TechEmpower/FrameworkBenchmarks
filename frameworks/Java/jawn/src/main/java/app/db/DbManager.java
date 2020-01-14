package app.db;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import javax.sql.DataSource;

import com.google.inject.Inject;
import com.google.inject.Singleton;

import app.helpers.Helper;
import app.models.Fortune;
import app.models.World;

@Singleton
public class DbManager {
    
    private static final String UPDATE_WORLD = "UPDATE world SET randomNumber = ? WHERE id= ?";
    private static final String SELECT_WORLD = "SELECT id, randomNumber FROM world WHERE id = ?";
    private static final String SELECT_FORTUNE = "SELECT id, message FROM fortune";

    private DataSource source;

    @Inject
    public DbManager(DataSource spec) {
        source = spec;
    }
    
    public World getWorld(int id) {
        try (Connection connection = source.getConnection()) {
            PreparedStatement statement = connection.prepareStatement(SELECT_WORLD, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);
            statement.setInt(1, id);
            ResultSet set = statement.executeQuery();
            
            // we always expect a response in this test environment
            //if (!set.next()) return null;
            set.next();
            
            return new World(set.getInt(1), set.getInt(2));
        } catch (SQLException e) {}
        return null;
    }
    
    public final World[] getWorlds(final int number) {
        World[] worlds = new World[number];
        
        try (final Connection connection = source.getConnection()) {
            
            for (int i = 0; i < number; i++) {
                try (final PreparedStatement statement = connection.prepareStatement(SELECT_WORLD, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
                
                    final int id = Helper.getRandomNumber();
                    
                    statement.setInt(1, id);
                    ResultSet set = statement.executeQuery();
                    set.next();
                    
                    worlds[i] = new World(id, set.getInt(2));
                }
            }
            
        } catch (SQLException e) {}
        
        return worlds;
    }
    
    public final World[] getAndUpdateWorlds(final int number) {
        World[] worlds = new World[number];
        
        try (final Connection connection = source.getConnection()) {
            
            for (int i = 0; i < number; i++) {
                try (
                    final PreparedStatement statement = connection.prepareStatement(SELECT_WORLD, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);
                    final PreparedStatement update = connection.prepareStatement(UPDATE_WORLD)) {
                
                    final int id = Helper.getRandomNumber(),
                         newRand = Helper.getRandomNumber();
                    
                    // get world
                    statement.setInt(1, id);
                    ResultSet set = statement.executeQuery();
                    set.next();
                    worlds[i] = new World(id, set.getInt(2));
                    
                    // update world
                    update.setInt(1, newRand);
                    update.setInt(2, id);
                    update.execute();
                    
                    // return updated world
                    worlds[i].randomNumber = newRand;
                }
            }
            
        } catch (SQLException e) {}
        
        return worlds;
    }
    
    public List<Fortune> fetchAllFortunes() {
        List<Fortune> list = new ArrayList<>();
        try (Connection connection = source.getConnection()) {
            PreparedStatement fetch = connection.prepareStatement(SELECT_FORTUNE, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);
            ResultSet set = fetch.executeQuery();
            while (set.next()) {
                list.add(new Fortune(set.getInt(1), escape(set.getString(2))));
            }
        } catch (SQLException ignore) { }
        return list;
    }
    
    private static final String escape(String html) {
        StringBuilder bob = new StringBuilder();
        char[] arr = html.toCharArray();
        for (char c : arr) {
            switch(c) {
            case '<':
                bob.append("&lt;");
                break;
            case '>':
                bob.append("&gt;");
                break;
            case '&':
                bob.append("&amp;");
                break;
            case '"':
                bob.append("&quot;");
                break;
            default:
                bob.append(c);
            }
        }
        return bob.toString();
    }
}
