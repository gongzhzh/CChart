import TSim.*;
import java.util.HashMap;
import java.util.concurrent.Semaphore;

public class Lab1 {
    final int semaphores = 9; // we can change this later if we do it with less.
    final int maxSpeed = 40;
    static final int DIRECTION_DOWN = TSimInterface.SWITCH_LEFT;
    static final int DIRECTION_UP = TSimInterface.SWITCH_RIGHT;
    final int [] terminalSensor = {1,5,15,17};    // Added the terminal sensor IDs

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
            Train trainA = new Train(1, DIRECTION_UP);
            Train trainB = new Train(2, DIRECTION_DOWN);

            Thread tA = new Thread(trainA);
            Thread tB = new Thread(trainB);
            tsi.setSpeed(1, speed1);
            tsi.setSpeed(2, speed2);
            tsi.setSwitch(15, 9, DIRECTION_DOWN);
            tsi.setSwitch(17, 7, DIRECTION_UP);     //test code, don't forget to remove it
            tsi.setSwitch(4, 9, DIRECTION_UP);
            tsi.setSwitch(3, 11, DIRECTION_UP); 
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
        private int trainSpeed;
        public Train(int id, int dir) {
          trainID = id;
          trainDir = dir;
        }

        //This function controls the behavior of the train
        public void run() {         // Gong is Working on this method - 1/31
            SensorEvent sensorEvent;
            int sensorID;
            while (true) {
                try {
                    sensorEvent = tsi.getSensor(trainID);
                    System.out.println("train" + this.trainID + " X:" + sensorEvent.getXpos()+ " Y:" + sensorEvent.getYpos());
                    sensorID = rail.GetSensorID(sensorEvent.getXpos(), sensorEvent.getYpos());
                    
                    rail.ReleaseSemaphore(sensorID, trainDir);


                    if (rail.IsTerminalSensor(sensorID, trainDir)) {
                        stop();
                        turnAround();
                        continue;
                    } 
                    //Try to acquire the next rail's semaphore
                    //aqcuireSem(sensorID, this.trainDir);
                    //switch
                    
                    

                } catch (CommandException e) {
                    e.printStackTrace();    // or only e.getMessage() for the error
                } catch (InterruptedException ex) {
                    ex.printStackTrace();    // or only e.getMessage() for the error
                }

            }
        }
        
        private int stop(){ //DONE(Ergi)
            try {
                tsi.setSpeed(this.trainID, 0);

            } catch (CommandException e) {
                e.printStackTrace();
            } 
            return 0;
        }

        private void turnAround(){  // TODO!
            if(this.trainDir == DIRECTION_UP){
                this.trainDir = DIRECTION_DOWN;
            } else{
                this.trainDir = DIRECTION_UP;
            }

        }
        
        private void aqcuireSem(int sensorID, int dir){
            try{
                int semID = Rail.getNextSemaphore(sensorID, dir);
                if (semID != -1) {
                    Semaphore s = semaphoresArr[semID];
                    s.acquire();
                    SwitchPoint(sensorID, dir);
                }
            }catch(InterruptedException ex){
                ex.printStackTrace();
            }
        }

        private void SwitchPoint(int sensorID, int dir){
            try {
                if (sensorID == 12 && dir == DIRECTION_DOWN) {
                    tsi.setSwitch(4, 9, DIRECTION_DOWN);
                }
                if  (sensorID == 18 && dir == DIRECTION_UP){
                    tsi.setSwitch(15, 9, DIRECTION_DOWN);
                }
                if (sensorID == 8 && dir == DIRECTION_DOWN) {
                    tsi.setSwitch(17, 7, DIRECTION_DOWN);
                }
                if (sensorID == 7 && dir == DIRECTION_UP) {
                    tsi.setSwitch(17, 7, DIRECTION_DOWN);
                }
                
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        
    }

    public class Rail{
        private final HashMap<String, Integer> sensorMap;

        public Rail() { //DONE(Ergi)
            sensorMap = new HashMap<>();
            // Mapped coordinates to sensor IDs. As soon as we finalize the sensor IDs
            // we can turn this into a for loop instead so that it's less code.
            sensorMap.put("16:3", 1);
            sensorMap.put("6:3", 2);
            sensorMap.put("6:7", 3);
            sensorMap.put("8:5", 4);
            sensorMap.put("16:5", 5);
            sensorMap.put("8:8", 6);
            sensorMap.put("10:7", 7);
            sensorMap.put("17:8", 8);
            sensorMap.put("19:7", 9);
            sensorMap.put("19:9", 10);
            sensorMap.put("15:10", 11);
            sensorMap.put("4:10", 12);
            sensorMap.put("1:9", 13);
            sensorMap.put("1:11", 14);
            sensorMap.put("16:11", 15);
            sensorMap.put("13:13", 16);
            sensorMap.put("16:13", 17);
        }

        public int GetSensorID(int x, int y) {  //DONE(Ergi)
        String key = x + ":" + y;
            Integer sensorID = sensorMap.get(key);
            if (sensorID == null) {
                System.out.println("Sensor not found for coordinates: " + key);
                return -1;  // Return an invalid sensor ID if the sesnor does not exist.
            }
            return sensorID;
        }

        //This method releases the last semaphore by sensor id and dirction of the train.
        public int ReleaseSemaphore(int sensorID, int dir) {            //TODO!
            return 0;        
        }

        // this method is for checking if the train is at the terminal it's supposed to be
        public boolean IsTerminalSensor(int sensorID, int dir) { //DONE(Ergi)
            if (dir == DIRECTION_DOWN) {
                // Check if sensorID matches one of the first two terminal sensor values.
                if (sensorID == terminalSensor[0] || sensorID == terminalSensor[1]) {
                    return true;
                }
            } else if (dir == DIRECTION_UP) {
                // Check if sensorID matches one of the last two terminal sensor values.
                if (sensorID == terminalSensor[2] || sensorID == terminalSensor[3]) {
                    return true;
                }
            }
            return false;
        }

        public static int getNextSemaphore(int sensorID, int dir){
            if (sensorID == 14 && dir == DIRECTION_UP) {
                return 0;
            }

            if (sensorID == 13 && dir == DIRECTION_UP) {
                return 2;
            }
            
            if (sensorID == 11 && dir == DIRECTION_UP) {
                return 4;
            }           
            return -1;
        }
    }
}
