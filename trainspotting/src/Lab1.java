import TSim.*;

public class Lab1 {

  public Lab1(Integer speed1, Integer speed2) {
    Train train1 = new Train(1, speed1);
    Train train2 = new Train(2, speed2);   
    train1.start();
    train2.start(); 
  }
}
