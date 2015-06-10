package app.controllers;

import java.util.Collections;
import java.util.List;

import net.javapla.jawn.core.ApplicationController;
import app.db.DbManager;
import app.models.Fortune;

import com.google.inject.Inject;

public class FortunesController extends ApplicationController {

    @Inject
    private DbManager db;
    
    public void index() {
        List<Fortune> fortunes = db.fetchAllFortunes();
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes, (f1, f2) -> f1.message.compareTo(f2.message));
        view("fortunes", fortunes);
    }
}
