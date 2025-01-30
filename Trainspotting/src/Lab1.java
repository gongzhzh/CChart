
import TSim.*;
import java.util.concurrent.Semaphore;

public class Lab1 {

    final int maxSpeed = 20;
    final int[][] sensorPos = {{1, 9}, {1, 11}, {3, 13}, {4, 10},
    {6, 3}, {6, 7}, {8, 5}, {8, 5}, {8, 8}, {10, 7}, {13, 11}, {13, 13},
    {14, 3}, {15, 5}, {15, 10}, {17, 8}, {19, 7}, {19, 9}};
    private TSimInterface tsi = TSimInterface.getInstance();
    private Semaphore[] semaphoresArr;

    public Lab1(int speed1, int speed2) {
        if (Math.abs(speed1) > maxSpeed || Math.abs(speed2) > maxSpeed) {
            System.out.println("Invalid Speed");
            return;
        }

        try {
            Train trainA = new Train(0);
            Train trainB = new Train(1);
            Thread tA = new Thread(trainA);
            Thread tB = new Thread(trainB);
            tsi.setSpeed(1, speed1);
            tsi.setSpeed(2, speed2);
            tA.start();
            tB.start();
        } catch (CommandException e) {
            e.printStackTrace();    // or only e.getMessage() for the error
            System.exit(1);
        }
    }

    public class Train implements Runnable {

        private int trainId;

        public Train(int id) {
          trainId = id;
        }

        public void run() {
            SensorEvent sensorEvent;
            while (true) {
                try {
                    sensorEvent = tsi.getSensor(trainId);
                    System.out.println("train" + trainId + " X:" + sensorEvent.getXpos()+ " Y:" + sensorEvent.getYpos());
                    


                } catch (CommandException e) {
                    e.printStackTrace();    // or only e.getMessage() for the error
                } catch (InterruptedException ex) {
                    ex.printStackTrace();    // or only e.getMessage() for the error
                }
            }
        }
    }
}
