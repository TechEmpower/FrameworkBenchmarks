package com.javanut.gl.benchmark;

public class FortuneObject implements Comparable<FortuneObject>{

	private int id;
	private String fortune;
	
	public int getId() {
		return id;
	}
	public void setId(int id) {
		this.id = id;
	}
	public String getFortune() {
		return fortune;
	}
	public void setFortune(String fortune) {
		this.fortune = fortune;
	}
	
	@Override
	public int compareTo(FortuneObject o) {
		return fortune.compareTo(o.fortune);
	}	
	
}
