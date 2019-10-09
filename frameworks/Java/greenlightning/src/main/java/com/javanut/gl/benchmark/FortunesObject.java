package com.javanut.gl.benchmark;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;


public class FortunesObject {

	private long connectionId; 
	private long sequenceId;
	private int status;
	private List<FortuneObject> list = new ArrayList<FortuneObject>(32); //non zero default
	private List<FortuneObject> recycle = new ArrayList<FortuneObject>(32);

	public long getConnectionId() {
		return connectionId;
	}


	public void setConnectionId(long connectionId) {
		this.connectionId = connectionId;
	}


	public long getSequenceId() {
		return sequenceId;
	}


	public void setSequenceId(long sequenceId) {
		this.sequenceId = sequenceId;
	}


	public int getStatus() {
		return status;
	}


	public void setStatus(int status) {
		this.status = status;
	}


	public void clear() {
		recycle.addAll(list);
		list.clear();
	}

	public void sort() {
		Collections.sort(list);
	}

	public List<FortuneObject> list() {		
		return list;
	}
	
	public void addFortune(int id, String fortune) {
		
		FortuneObject obj; //This eliminates any GC by recycling the old Fortune Objects to be repopulated with new data.
		if (recycle.isEmpty()) {
			obj = new FortuneObject(); 
		} else {
			obj = recycle.remove(recycle.size()-1);			
		}
		
		obj.setId(id);
		obj.setFortune(fortune);
		list.add(obj);

	}

		
}
