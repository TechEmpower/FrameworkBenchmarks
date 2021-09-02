/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import java.util.Random;

/**
 *
 * @author zhangjx
 */
public class RedRandom extends Random {

    private long s0, s1;

    public RedRandom() {
        this.s0 = super.nextLong();
        this.s1 = super.nextLong();
    }

    @Override
    public long nextLong() {
        final long s0 = this.s0;
        long s1 = this.s1;
        final long result = s0 + s1;
        s1 ^= s0;
        this.s0 = Long.rotateLeft(s0, 24) ^ s1 ^ s1 << 16;
        this.s1 = Long.rotateLeft(s1, 37);
        return result;
    }

    @Override
    public int nextInt() {
        return (int) nextLong();
    }

    @Override
    public int nextInt(int bound) {
        long s = nextLong();
        return (int) (s < 0 ? -s : s) % bound;
    }

}
