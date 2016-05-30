module FrameworkBench {
	struct Message {
		String message;
	}
	sql World from world(id) {
		int id;
		int randomNumber from randomnumber;
	}
	sql Fortune from fortune {
	    int id;
	    String message;
	}
} 
