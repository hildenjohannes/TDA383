import java.util.*;
import java.util.concurrent.Semaphore;

import TSim.CommandException;
import TSim.TSimInterface;
import TSim.SensorEvent;

public class Train extends Thread {

	private int id;
	private int speed;
	private static Semaphore [] sem;
	private TSimInterface tsi = TSimInterface.getInstance();
	private List<Position> posList;
	private boolean headingNorth;

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

		if (id == 1) {
			headingNorth = false;
		} else {
			headingNorth = true;
		}

		if (sem == null) {
			sem = new Semaphore [6];
			for (int i = 0; i < 6; i++) {
				sem[i] = new Semaphore(1);
			}
		}

		posList.add(new Position(14,3));

		posList.add(new Position(8,5));
		posList.add(new Position(14,5));

		posList.add(new Position(6,7));
		posList.add(new Position(10,7));

		posList.add(new Position(15,7));
		posList.add(new Position(19,7));

		posList.add(new Position(9,8));
		posList.add(new Position(16,8));

		posList.add(new Position(2,9));
		posList.add(new Position(6,9));
		posList.add(new Position(13,9));
		posList.add(new Position(17,9));

		posList.add(new Position(5,10));
		posList.add(new Position(14,10));

		posList.add(new Position(1,11));
		posList.add(new Position(5,11));
		posList.add(new Position(14,11));

		posList.add(new Position(3,13));
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
				SensorEvent sEvent = tsi.getSensor(id);

				if (sEvent.getStatus() == sEvent.ACTIVE) {

				  if (sEvent.getXpos() == 15 && sEvent.getYpos() == 7) {
					  if (headingNorth) {
						  sem[2].release();
					  } else {
						  tsi.setSpeed(id,0);
						  sem[2].acquire();
						  tsi.setSpeed(id, speed);
						  tsi.setSwitch(17, 7, tsi.SWITCH_RIGHT);
					  }

				  }

				  if (sEvent.getXpos() == 17 && sEvent.getYpos() == 9) {
					  if (headingNorth) {
						  sem[3].release();
					  } else {
						  if (sem[3].tryAcquire()) {
							  tsi.setSwitch(15, 9, tsi.SWITCH_RIGHT);
						  } else {
							  tsi.setSwitch(15, 9, tsi.SWITCH_LEFT);
						  }
					  }
				  }
				  
				  if (sEvent.getXpos() == 16 && sEvent.getYpos() == 8) {
					  if (headingNorth) {
						  sem[2].release();
					  } else {
						  tsi.setSpeed(id,0);			
						  sem[2].acquire();
						  tsi.setSpeed(id, speed);
						  tsi.setSwitch(15, 9, tsi.SWITCH_LEFT);
					  }
				
				  } 
				  

				  if (sEvent.getXpos() == 14 && sEvent.getYpos() == 10) {
					  if (headingNorth) {
						  tsi.setSpeed(id,0);
						  sem[2].acquire();
						  tsi.setSpeed(id, speed);
						  tsi.setSwitch(15, 9, tsi.SWITCH_LEFT);
					  } else {
						  sem[2].release();
					  }
				  }

				  if (sEvent.getXpos() == 2 && sEvent.getYpos() == 9) {
					  if (headingNorth) {
						  if (sem[3].tryAcquire()) {
							  tsi.setSwitch(4, 9, tsi.SWITCH_LEFT);
						  } else {
							  tsi.setSwitch(4, 9, tsi.SWITCH_RIGHT);
						  }
					  } else {
						  sem[3].release();
					  }
				  }

				  
				  if (sEvent.getXpos() == 6 && sEvent.getYpos() == 9) {
					  if (headingNorth) {
						  sem[4].release();
					  } else {
						  tsi.setSpeed(id,0);			
						  sem[4].acquire();
						  tsi.setSpeed(id, speed);
						  tsi.setSwitch(4, 9, tsi.SWITCH_LEFT);
					  }	
				  } 
				  
				  if (sEvent.getXpos() == 5 && sEvent.getYpos() == 11) {
					  if (headingNorth) {
						  tsi.setSpeed(id,0);			
						  sem[4].acquire();
						  tsi.setSpeed(id, speed);
						  tsi.setSwitch(3, 11, tsi.SWITCH_LEFT);
					  } else {
						  sem[4].release();
					  }	
				  } 
				  
				  if (sEvent.getXpos() == 14 && sEvent.getYpos() == 11) {
					  if (!headingNorth) {
						  changeDir();
					  }	
				  } 
				  
				  if (sEvent.getXpos() == 14 && sEvent.getYpos() == 13) {
					  if (!headingNorth) {
						  changeDir();
					  }	
				  } 
				  
				  if (sEvent.getXpos() == 14 && sEvent.getYpos() == 3) {
					  if (headingNorth) {
						  changeDir();
					  }	
				  } 
				  
				  if (sEvent.getXpos() == 14 && sEvent.getYpos() == 5) {
					  if (headingNorth) {
						  changeDir();
					  }	
				  } 
				  
				  
				  if (sEvent.getXpos() == 5 && sEvent.getYpos() == 10) {
					  if (headingNorth) {
						  sem[4].release();
					  } else {
							tsi.setSpeed(id,0);
						  sem[4].acquire();
						  tsi.setSpeed(id, speed);
						  tsi.setSwitch(4, 9, tsi.SWITCH_RIGHT);
					  }
				  }

					if (sEvent.getXpos() == 1 && sEvent.getYpos() == 11) {
						if (headingNorth) {
							sem[5].release();
						} else {
							if (sem[5].tryAcquire()) {
							  tsi.setSwitch(3, 11, tsi.SWITCH_LEFT);
						  } else {
							  tsi.setSwitch(4, 9, tsi.SWITCH_RIGHT);
						  }
						}
					}

					if (sEvent.getXpos() == 3 && sEvent.getYpos() == 13) {
						if (headingNorth) {
							tsi.setSpeed(id,0);
 						 	sem[4].acquire();
 						 	tsi.setSpeed(id, speed);
 						 	tsi.setSwitch(3, 11, tsi.SWITCH_RIGHT);
						} else {
							sem[4].release();
						}
					}

					if (sEvent.getXpos() == 10 && sEvent.getYpos() == 7) {
						if (headingNorth) {
							tsi.setSpeed(id,0);
 						 	sem[1].acquire();
 						 	tsi.setSpeed(id, speed);
						} else {
							sem[1].release();
						}
					}

					if (sEvent.getXpos() == 9 && sEvent.getYpos() == 8) {
						if (headingNorth) {
							tsi.setSpeed(id,0);
 						 	sem[1].acquire();
 						 	tsi.setSpeed(id, speed);
						} else {
							sem[1].release();
						}
					}

					if (sEvent.getXpos() == 6 && sEvent.getYpos() == 7) {
						if (headingNorth) {
							sem[1].release();
						} else {
							tsi.setSpeed(id,0);
 						 	sem[1].acquire();
 						 	tsi.setSpeed(id, speed);
						}
					}

					if (sEvent.getXpos() == 8 && sEvent.getYpos() == 5) {
						if (headingNorth) {
							sem[1].release();
						} else {
							tsi.setSpeed(id,0);
 						 	sem[1].acquire();
 						 	tsi.setSpeed(id, speed);
						}
					}


				  if (sEvent.getXpos() == 13 && sEvent.getYpos() == 9) {

					  if (headingNorth) {
						  tsi.setSpeed(id,0);
						  sem[2].acquire();
						  tsi.setSpeed(id, speed);
						  tsi.setSwitch(15, 9, tsi.SWITCH_RIGHT);
					  } else {
						  sem[2].release();
					  }


				  }
				  if ((sEvent.getXpos() == 18 && sEvent.getYpos() == 7) && headingNorth) {
					  tsi.setSwitch(17, 7, tsi.SWITCH_RIGHT);
					  sem[2].release();

				  }
				}

		//		  posList.contains(o)





			    }
			    catch (Exception e) {
			      e.printStackTrace();    // or only e.getMessage() for the error
			      System.exit(1);
			    }
		}
	}
	
	private void changeDir() {
		speed = -speed;
		headingNorth ^= true;
	}
	
	/**
	private void hej(SensorEvent e, int xPos, int yPos) {

		if ((e.getXpos() == xPos && e.getYpos() == yPos) && !headingNorth) {
			  System.out.println("blablablabl");
			  tsi.setSpeed(id,0);
			  System.out.println(sem[2].availablePermits());
			  sem[2].acquire();
			  System.out.println("då");
			  tsi.setSpeed(id, speed);
			  tsi.setSwitch(17, 7, tsi.SWITCH_RIGHT);
		  }






	} */
}
