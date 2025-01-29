import TSim.*;

public class Lab1 {
  final int maxSpeed = 20;
  private int[][] sensorPos;



  public Lab1(int speed1, int speed2) {
    if(speed1 > maxSpeed || speed2 > maxSpeed){
      System.out.println("Invalid Speed");
      return;
    }

    TSimInterface tsi = TSimInterface.getInstance();

    try {
      tsi.setSpeed(1,speed1);
      tsi.setSpeed(2,speed2);
    }
    catch (CommandException e) {
      e.printStackTrace();    // or only e.getMessage() for the error
      System.exit(1);
    }
  }

  public class Train implements Runnable{
    public int run()
    {
      return 0;
    }
  }
}
