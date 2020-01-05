package com.example.helloworld;

import com.example.helloworld.resources.Helper;

public class Test {

	public static void main(String[] args) {
		int[] ids = Helper.getRandomInts(20);
		for (int id : ids) {
			int newNumber;
			do {
				newNumber = Helper.randomWorld();
				break;
			} while (true);
			System.out.println(id + ":" + newNumber);
		}

	}

}
