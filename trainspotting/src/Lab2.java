import TSim.CommandException;
import TSim.SensorEvent;
import TSim.TSimInterface;

import java.util.Collections;
import java.util.Date;
import java.util.LinkedList;
import java.util.Queue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.*;

public class Lab2 {

    private static SectionMonitor[] mon;

    public Lab2(Integer speed1, Integer speed2) {

        if (mon == null) {
            mon = new SectionMonitor[6];
            for (int i = 0; i < 6; i++) {
                mon[i] = new SectionMonitor();
            }
        }

        Train train1 = new Train(1, speed1);
        Train train2 = new Train(2, speed2);
        train1.start();
        train2.start();
    }

    /**
     * A monitor
     */
    private class SectionMonitor {

    	private boolean blocked;
    	private Lock lock;
    	private Condition cond;
    	
    	public SectionMonitor() {
    		lock = new ReentrantLock();
    		cond = lock.newCondition();
    	}
    	
    	/**
    	 * Enter a critical section, if blocked, thread will wait.
    	 * @throws InterruptedException Fails to wait.
    	 */
    	public void enter() throws InterruptedException {
    		lock.lock();
    		if (blocked) {
    			cond.await();
    		}
    		blocked = true;
    		lock.unlock();
    	}
    	
    	/**
    	 * Try entering a critical section.
    	 * @return if entered
    	 */
    	public boolean tryEnter() {
    		lock.lock();
    		if (!blocked) {
    			blocked = true;
    			lock.unlock();
    			return true;
    		}
    		lock.unlock();
    		return false;
    	}
    	
    	/**
    	 * Leave a critical section.
    	 */
    	public void leave() {
    		lock.lock();
    		blocked = false;
    		cond.signal();
    		lock.unlock();
    	}
    }
    
    /**
     * A train with an ID and a speed
     */
    private class Train extends Thread {

        private int id;
        private int speed;
        private TSimInterface tsi = TSimInterface.getInstance();
        private boolean headingNorth; //direction of train

        public Train(int id, int speed) {
            this.id = id;
            this.speed = speed;

            if (id == 1) {
                headingNorth = false;
            } else {
                headingNorth = true;
            }
        }

        @Override
        public void run() {
            try {
                tsi.setSpeed(id, speed);

                while (true) {
                    SensorEvent sEvent = tsi.getSensor(id);
                    if (sEvent.getStatus() == sEvent.ACTIVE) { //only active sensors are interesting.

                        //Specific if-cases for each sensor
                        if (sEvent.getXpos() == 15 && sEvent.getYpos() == 7) {
                            if (headingNorth) {
                                leaveSection(2);
                            } else {
                                enterSectionAndSwitch(2, 17, 7, tsi.SWITCH_RIGHT);
                            }
                        }

                        if (sEvent.getXpos() == 18 && sEvent.getYpos() == 9) {
                            if (headingNorth) {
                                leaveSection(3);
                            } else {
                                enterAtTrackSplit(3, 15, 9, tsi.SWITCH_RIGHT, tsi.SWITCH_LEFT);
                            }
                        }

                        if (sEvent.getXpos() == 15 && sEvent.getYpos() == 8) {
                            if (headingNorth) {
                                leaveSection(2);
                            } else {
                                enterSectionAndSwitch(2, 17, 7, tsi.SWITCH_LEFT);
                            }
                        }

                        if (sEvent.getXpos() == 13 && sEvent.getYpos() == 10) {
                            if (headingNorth) {
                                enterSectionAndSwitch(2, 15, 9, tsi.SWITCH_LEFT);
                            } else {
                                leaveSection(2);
                            }
                        }

                        if (sEvent.getXpos() == 2 && sEvent.getYpos() == 9) {
                            if (headingNorth) {
                                enterAtTrackSplit(3, 4, 9, tsi.SWITCH_LEFT, tsi.SWITCH_RIGHT);
                            } else {
                                leaveSection(3);
                            }
                        }

                        if (sEvent.getXpos() == 7 && sEvent.getYpos() == 9) {
                            if (headingNorth) {
                                leaveSection(4);
                            } else {
                                enterSectionAndSwitch(4, 4, 9, tsi.SWITCH_LEFT);
                            }
                        }

                        if (sEvent.getXpos() == 5 && sEvent.getYpos() == 11) {
                            if (headingNorth) {
                                enterSectionAndSwitch(4, 3, 11, tsi.SWITCH_LEFT);
                            } else {
                                leaveSection(4);
                            }
                        }

                        if (sEvent.getXpos() == 14 && sEvent.getYpos() == 11) {
                            if (!headingNorth) {
                                changeDir();
                            } else {
                                if (!mon[5].blocked) { //fix to enter initial monitor
                                    mon[5].enter();
                                }
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
                            } else {
                                if (!mon[0].blocked) { //fix to enter initial monitor
                                    mon[0].enter();
                                }
                            }
                        }

                        if (sEvent.getXpos() == 14 && sEvent.getYpos() == 5) {
                            if (headingNorth) {
                                changeDir();
                            }
                        }

                        if (sEvent.getXpos() == 6 && sEvent.getYpos() == 10) {
                            if (headingNorth) {
                                leaveSection(4);
                            } else {
                                enterSectionAndSwitch(4, 4, 9, tsi.SWITCH_RIGHT);
                            }
                        }

                        if (sEvent.getXpos() == 1 && sEvent.getYpos() == 11) {
                            if (headingNorth) {
                                leaveSection(5);
                            } else {
                                enterAtTrackSplit(5, 3, 11, tsi.SWITCH_LEFT, tsi.SWITCH_RIGHT);
                            }
                        }

                        if (sEvent.getXpos() == 4 && sEvent.getYpos() == 13) {
                            if (headingNorth) {
                                enterSectionAndSwitch(4, 3, 11, tsi.SWITCH_RIGHT);
                            } else {
                                leaveSection(4);
                            }
                        }

                        if (sEvent.getXpos() == 10 && sEvent.getYpos() == 7) {
                            if (headingNorth) {
                                enterSection(1);
                            } else {
                                leaveSection(1);
                            }
                        }

                        if (sEvent.getXpos() == 10 && sEvent.getYpos() == 8) {
                            if (headingNorth) {
                                enterSection(1);
                            } else {
                                leaveSection(1);
                            }
                        }

                        if (sEvent.getXpos() == 6 && sEvent.getYpos() == 7) {
                            if (headingNorth) {
                                leaveSection(1);
                            } else {
                                enterSection(1);
                            }
                        }

                        if (sEvent.getXpos() == 8 && sEvent.getYpos() == 5) {
                            if (headingNorth) {
                                leaveSection(1);
                            } else {
                                enterSection(1);
                            }
                        }

                        if (sEvent.getXpos() == 12 && sEvent.getYpos() == 9) {
                            if (headingNorth) {
                                enterSectionAndSwitch(2, 15, 9, tsi.SWITCH_RIGHT);
                            } else {
                                leaveSection(2);
                            }

                        }
                        if ((sEvent.getXpos() == 19 && sEvent.getYpos() == 7)) {
                            if (headingNorth) {
                                enterAtTrackSplit(0, 17, 7, tsi.SWITCH_RIGHT, tsi.SWITCH_LEFT);
                            } else {
                                leaveSection(0);
                            }
                        }
                    }
                }
            } catch (CommandException e) {
                e.printStackTrace();
                System.exit(1);
            } catch (InterruptedException e) {
                e.printStackTrace();
                System.exit(1);
            }
        }

        /**
         * Train enters a critical section. Will enter the monitor corresponding to the section.
         *
         * @param sectionId 			The id of the section/monitor
         * @throws CommandException     Fails to set speed
         * @throws InterruptedException Fails to enter monitor
         */
        private void enterSection(int sectionId) throws CommandException, InterruptedException {
            tsi.setSpeed(id, 0);
            mon[sectionId].enter();
            tsi.setSpeed(id, speed);
        }

        /**
         * Train enters a critical section and changes orientation of a switch.
         * Will enter the monitor corresponding to the section.
         *
         * @param sectionId         	The id of the section/monitor
         * @param x                 	The x-position of the switch
         * @param y                 	The y-position of the switch
         * @param switchOrientation 	Orientation of the switch
         * @throws CommandException     Fails to set speed or switch
         * @throws InterruptedException Fails to enter monitor
         */
        private void enterSectionAndSwitch(int sectionId, int x, int y, int switchOrientation) throws CommandException, InterruptedException {
            enterSection(sectionId);
            tsi.setSwitch(x, y, switchOrientation);
        }

        /**
         * Train needs to choose track depending on if one of the tracks are occupied.
         * Will enter the monitor corresponding to the section if the default track is availible.
         *
         * @param sectionId          The id of the section/monitor
         * @param x                  The x-position of the switch
         * @param y                  The y-position of the switch
         * @param firstChoiceSwitch  Orientation of switch if train is allowed to travel the default way
         * @param secondChoiceSwitch Orientation of switch otherwise
         * @throws CommandException  Fails to set switch
         */
        private void enterAtTrackSplit(int sectionId, int x, int y, int firstChoiceSwitch, int secondChoiceSwitch) throws CommandException {
            if (mon[sectionId].tryEnter()) {        	
                tsi.setSwitch(x, y, firstChoiceSwitch);
            } else {
                tsi.setSwitch(x, y, secondChoiceSwitch);
            }
        }

        /**
         * Train leaves section. Will leave the monitor corresponding to the section.
         *
         * @param sectionId The id of the section/monitor
         */
        private void leaveSection(int sectionId) {
        	mon[sectionId].leave();
            
        }

        /**
         * Changes direction of train e.g. inverts the sign of the velocity.
         * Train will first stop before it changes direction.
         *
         * @throws CommandException     Fails to set speed
         * @throws InterruptedException Fails to sleep thread
         */
        private void changeDir() throws CommandException, InterruptedException {
            tsi.setSpeed(id, 0);
            this.sleep((long) (1000 + Math.abs(speed) * 20));
            speed = -speed;
            tsi.setSpeed(id, speed);
            if (headingNorth) {
                headingNorth = false;
            } else {
                headingNorth = true;
            }
        }
    }
}