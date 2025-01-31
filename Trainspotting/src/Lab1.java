
import TSim.*;
import java.util.concurrent.Semaphore;

public class Lab1 {
    final int semaphores = 9; // we can change this later if we do it with less.
    final int maxSpeed = 20;
    final int DIRECTION_UP = TSimInterface.SWITCH_LEFT;
    final int DIRECTION_DOWN = TSimInterface.SWITCH_RIGHT;
    final int [] terminalSensor = {1,2,3,4};    //TODO! modify this later
    final int[][] sensorPos = {{1, 9}, {1, 11}, {3, 13}, {4, 10},
    {6, 3}, {6, 7}, {8, 5}, {8, 5}, {8, 8}, {10, 7}, {13, 11}, {13, 13},
    {14, 3}, {15, 5}, {15, 10}, {17, 8}, {19, 7}, {19, 9}};
    private TSimInterface tsi = TSimInterface.getInstance();
    private Semaphore[] semaphoresArr = new Semaphore[semaphores];
    private Rail rail = new Rail();

    public Lab1(int speed1, int speed2) {
        if (Math.abs(speed1) > maxSpeed || Math.abs(speed2) > maxSpeed) {
            System.out.println("Invalid Speed");
            return;
        }

        // This initializes the semaphores
        for (int i = 0; i < semaphoresArr.length; i++) {
        // 1 permit means that only one train can pass at a time.
        semaphoresArr[i] = new Semaphore(1);
    }

        try {
            Train trainA = new Train(0, DIRECTION_UP);
            Train trainB = new Train(1, DIRECTION_DOWN);
            Thread tA = new Thread(trainA);
            Thread tB = new Thread(trainB);
            tsi.setSpeed(1, speed1);
            tsi.setSpeed(2, speed2);
            tsi.setSwitch(15, 9, DIRECTION_DOWN);     //test code, don't forget to remove it
            tA.start();
            tB.start();
        } catch (CommandException e) {
            e.printStackTrace();    // or only e.getMessage() for the error
            System.exit(1);
        }
    }
    
    public class Train implements Runnable {

        private int trainID;
        private int trainDir;
        public Train(int id, int dir) {
          trainID = id;
          trainDir = dir;
        }

        //This function controls the 
        public void run() {         // Gong is Working on this method - 1/31
            SensorEvent sensorEvent;
            int sensorID;
            while (true) {
                try {
                    sensorEvent = tsi.getSensor(trainID);
                    //System.out.println("train" + trainId + " X:" + sensorEvent.getXpos()+ " Y:" + sensorEvent.getYpos());
                    sensorID = rail.GetSensorID(sensorEvent.getXpos(), sensorEvent.getYpos());
                    rail.ReleaseSemaphore(sensorID, trainDir);
                    if (rail.IsTerminalSensor(sensorID)){
                        stop();
                    }
                    

                } catch (CommandException e) {
                    e.printStackTrace();    // or only e.getMessage() for the error
                } catch (InterruptedException ex) {
                    ex.printStackTrace();    // or only e.getMessage() for the error
                }

            }
        }

        private int stop(){
            return 0;
        }
    }

    public class Rail{
        public int GetSensorID(int x, int y) {  //TODO!
            return 0;
        }

        //This method releases the next semaphore by sensor id and dirction of the train.
        public int ReleaseSemaphore(int sensorID, int dir) {            //TODO!
            return 0;        
        }

        public boolean IsTerminalSensor(int sensorID){     //TODO!
            return false;
        }

    }
}
