package controllers;

import dao.WorldDao;
import model.World;

import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

import ninja.Result;
import ninja.Results;

import com.google.inject.Inject;
import com.google.inject.Singleton;
import dao.SetupDao;

@Singleton
public class SetupController {

    @Inject
    SetupDao setupDao;

    public Result setupData() {
        
        setupDao.deleteAllData();
        
        setupDao.generateWorldsForTest();
        setupDao.generateFortunesForTest();
        
        return Results.text().render("setup done");
        
    }
    
   

}
