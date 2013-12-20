package hello.controllers;

import hello.model.Fortune;

import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.Query;

import com.google.inject.Inject;
import com.google.inject.Provider;
import com.google.inject.Singleton;
import com.google.inject.persist.Transactional;
import hello.model.World;

/**
 * This class is just for testing.
 * Has nothing to do with the TechEmpower test itself...
 * @author ra
 */
@Singleton
public class SetupDao {

    @Inject
    Provider<EntityManager> entitiyManagerProvider;

    @Transactional
    public void generateWorldsForTest() {
	
        for (int i = 0; i < 10000; i++) {
            
            World world = new World();
            world.randomNumber = i; // not really a random number. But we can test with that...
            entitiyManagerProvider.get().persist(world);
            
         }
        
        
    }
    
    @Transactional
    public void generateFortunesForTest() {
	

        {
            
            Fortune fortune = new Fortune();
            // dummy message => just to make sure utf-8 works.
            fortune.message = "レームワークのベンチマーク";
            entitiyManagerProvider.get().persist(fortune);
            
        }
        
        {
 
            Fortune fortune = new Fortune();
            // dummy message => just to make sure utf-8 works.
            fortune.message = "<script>I want to be escaped</script>";
            entitiyManagerProvider.get().persist(fortune);
            
        }   
   
        
        
    }

}
