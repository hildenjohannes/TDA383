import java.util.*;

import TSim.CommandException; 
import TSim.TSimInterface;
import TSim.SensorEvent;

public class Train extends Thread {
	
	private int id;
	private int speed;
	private TSimInterface tsi = TSimInterface.getInstance();
	private List<Position> posList;
	
	private class Position {
		
		public int x;
		public int y;
		
		public Position (int xPos, int yPos) {
			this.x = xPos;
			this.y = yPos;
		}
		
		public boolean equals(Object other) {
			if (other == null) {
				return false;
			} else if (other == this) {
				return true;
			} else if (!(other instanceof Position)) {
				return false;
			} else {
				return (((Position)other).x == this.x && ((Position)other).y == this.y);
			}
		}
	}
	
	public Train (int id, int speed) {
		this.id = id;
		this.speed = speed;
		this.posList = new ArrayList<Position>();
		
		posList.add(new Position(8,6));
		posList.add(new Position(7,7));
		posList.add(new Position(11,7));
		posList.add(new Position(9,8));
		posList.add(new Position(14,7));
		posList.add(new Position(18,7));
		posList.add(new Position(16,8));
		posList.add(new Position(16,9));
		posList.add(new Position(14,10));
		posList.add(new Position(14,9));
		posList.add(new Position(5,10));
		posList.add(new Position(5,9));
		posList.add(new Position(3,9));
		posList.add(new Position(4,11));
		posList.add(new Position(2,11));
		posList.add(new Position(3,12));
		posList.add(new Position(14,3));
		posList.add(new Position(14,5));
		posList.add(new Position(14,11));
		posList.add(new Position(14,13));
		
		try {
	      tsi.setSpeed(id,speed);
	    }
	    catch (CommandException e) {
	      e.printStackTrace();    // or only e.getMessage() for the error
	      System.exit(1);
	    }
	}

	public void run() {
		while (true) {
			try {
				  System.out.println("run");
				  SensorEvent sEvent = tsi.getSensor(1);
				  
				  System.out.println(sEvent.getXpos() + " " + sEvent.getYpos());
				  			
				  if (sEvent.getXpos() == 11 && sEvent.getYpos() == 7) {
					  tsi.setSwitch(17, 7, tsi.SWITCH_RIGHT);
				  } 
				  
		//		  posList.contains(o)
				  
				  
				  
				  
			      
			    }
			    catch (Exception e) {
			      e.printStackTrace();    // or only e.getMessage() for the error
			      System.exit(1);
			    }    
		}
	}
}
