package com.te;

import ro.pippo.core.Pippo;

/**
 * Run application from here.
 */
public class PippoLauncher {

    public static void main(String[] args) {
        Pippo pippo = new Pippo(new PippoApplication());
        pippo.start(8000);
    }

}
