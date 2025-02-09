import TSim.*;
import java.util.AbstractMap;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Semaphore;

public class Lab1 {

    final int semaphores = 9; // we can change this later if we do it with less.
    final int maxSpeed = 20;
    static final int DIRECTION_DOWN = TSimInterface.SWITCH_LEFT;
    static final int DIRECTION_UP = TSimInterface.SWITCH_RIGHT;
    final int SWITCH_POINT_1_X = 3, SWITCH_POINT_1_Y = 11;
    final int SWITCH_POINT_2_X = 4, SWITCH_POINT_2_Y = 9;
    final int SWITCH_POINT_3_X = 15, SWITCH_POINT_3_Y = 9;
    final int SWITCH_POINT_4_X = 17, SWITCH_POINT_4_Y = 7;
    final int terminalSensor[] = {1, 5, 15, 17};    // Added the terminal sensor IDs

    private TSimInterface tsi = TSimInterface.getInstance();
    private final Semaphore[] semaphoresArr = new Semaphore[semaphores];
    private Rail rail = new Rail();

    // Moved this hashmap out, otherwise this can't be accessed by train objects
    private static final HashMap<String, Integer> sensorMap = new HashMap<>();

    private void initializeSensors() {
        // Mapped coordinates to sensor IDs. As soon as we finalize the sensor IDs
        // we can turn this into a for loop instead so that it's less code.
        sensorMap.put("13:3", 1);
        sensorMap.put("7:3", 2);
        sensorMap.put("6:6", 3);
        sensorMap.put("10:5", 4);
        sensorMap.put("15:5", 5);
        sensorMap.put("10:8", 6);
        sensorMap.put("11:7", 7);
        sensorMap.put("15:8", 8);
        sensorMap.put("19:9", 9);
        sensorMap.put("14:7", 10);
        sensorMap.put("11:10", 11);
        sensorMap.put("7:10", 12);
        sensorMap.put("1:10", 13);
        sensorMap.put("6:11", 14);
        sensorMap.put("13:11", 15);
        sensorMap.put("6:13", 16);
        sensorMap.put("13:13", 17);
        sensorMap.put("11:9", 18);
        sensorMap.put("7:9", 19);
    }

    private static final HashMap<Map.Entry<Integer, Integer>, Integer> semaphoreAcquireMap = new HashMap<>();

    private void initializeSemaphores() {
        // Initialize the semaphores
        for (int i = 0; i < semaphoresArr.length; i++) {
            // 1 permit means that only one train can pass at a time.
            semaphoresArr[i] = new Semaphore(1);
        }
        // Acquire the semaphores for the trains that start at the terminal sensors
        semaphoresArr[7].tryAcquire();

        // semaphore 8
        semaphoreAcquireMap.put(new AbstractMap.SimpleEntry<>(13, DIRECTION_UP), 7);
        // semaphore 7
        semaphoreAcquireMap.put(new AbstractMap.SimpleEntry<>(2, DIRECTION_UP), 6);
        semaphoreAcquireMap.put(new AbstractMap.SimpleEntry<>(5, DIRECTION_UP), 6);
        semaphoreAcquireMap.put(new AbstractMap.SimpleEntry<>(8, DIRECTION_DOWN), 6);
        semaphoreAcquireMap.put(new AbstractMap.SimpleEntry<>(10, DIRECTION_DOWN), 6);
        // semaphore 6
        semaphoreAcquireMap.put(new AbstractMap.SimpleEntry<>(9, DIRECTION_DOWN), 5);
        // semaphore 5  no corisponding semaphore
        // semaphore 4
        semaphoreAcquireMap.put(new AbstractMap.SimpleEntry<>(18, DIRECTION_DOWN), 3);
        semaphoreAcquireMap.put(new AbstractMap.SimpleEntry<>(10, DIRECTION_UP), 3);
        semaphoreAcquireMap.put(new AbstractMap.SimpleEntry<>(8, DIRECTION_UP), 3);
        semaphoreAcquireMap.put(new AbstractMap.SimpleEntry<>(11, DIRECTION_DOWN), 3);
        // semaphore 3
        semaphoreAcquireMap.put(new AbstractMap.SimpleEntry<>(13, DIRECTION_DOWN), 2);
        // semaphore 2
        semaphoreAcquireMap.put(new AbstractMap.SimpleEntry<>(9, DIRECTION_UP), 1);
        // semaphore 1
        semaphoreAcquireMap.put(new AbstractMap.SimpleEntry<>(16, DIRECTION_DOWN), 0);
        semaphoreAcquireMap.put(new AbstractMap.SimpleEntry<>(14, DIRECTION_DOWN), 0);
        semaphoreAcquireMap.put(new AbstractMap.SimpleEntry<>(12, DIRECTION_UP), 0);
        semaphoreAcquireMap.put(new AbstractMap.SimpleEntry<>(19, DIRECTION_UP), 0);
    }

    public int GetSensorSeq(int x, int y) {  //DONE(Ergi)
        String key = x + ":" + y;
        Integer sensorID = sensorMap.get(key);
        if (sensorID == null) {
            return -1;  // Return an invalid sensor ID if the sesnor does not exist.
        }
        return sensorID;
    }

    public Integer getForkSemaphoreIdx(int semaphoreIdx, int dir) {
        if (dir == DIRECTION_DOWN) {
            return switch (semaphoreIdx) {
                case 2 -> 1;
                case 5 -> 4;
                 default -> -1;
             };
        } else {
             return switch (semaphoreIdx) {
            case 1 -> 2;
            case 7 -> 8;
            default -> -1;
             };
        }
    }

    public Lab1(int speed1, int speed2) {
        if (Math.abs(speed1) > maxSpeed || Math.abs(speed2) > maxSpeed) return;
        initializeSemaphores();
        initializeSensors();
        Train trainA = new Train(1, DIRECTION_UP);
        Train trainB = new Train(2, DIRECTION_DOWN);
        Thread tA = new Thread(trainA);
        Thread tB = new Thread(trainB);
        try {
            trainA.setSpeed(speed1);
            trainB.setSpeed(speed2);
            tA.start();
            tB.start();
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
    }

    public class Train implements Runnable {
        private final int trainID;
        private int trainDir, trainSpeed;
        private int currSemIdx = -1;
        private int prevSemIdx = -1;
        private int holdForkSemIdx = -1;
        private boolean isFirst = true;
        public Train(int id, int dir) {
            trainID = id;
            trainDir = dir;
        }

        public void setSpeed(int speed) throws CommandException {
            tsi.setSpeed(this.trainID, speed);
            this.trainSpeed = speed;
        }

        //This function controls the behavior of the train
        public void run() {         
            SensorEvent sensorEvent;
            int sensorSeq;
            while (true) {
                try {
                    sensorEvent = tsi.getSensor(trainID);
                    if (sensorEvent.getStatus() != SensorEvent.ACTIVE) continue;
                    sensorSeq = GetSensorSeq(sensorEvent.getXpos(), sensorEvent.getYpos());
                    
                    // the terminal sensor is the first sensor the train passes, so we set the previous semaphore index to 7 the first time the train passes the terminal sensor
                    if (sensorSeq == 15 && isFirst) {
                        this.currSemIdx = 7;
                        isFirst = false;
                    }

                    // if ((sensorSeq == 15 || sensorSeq == 17) && this.trainDir == DIRECTION_DOWN)    // If the train is entering the terminal, no need to aquire/release the semaphore
                    //     continue;

                    if (rail.IsTerminalSensor(sensorSeq, trainDir)) {
                        stop();
                        turnAround();
                        continue;
                    }

                    if (ReleaseSemaphore(sensorSeq, trainDir, this.prevSemIdx) == 0)
                        this.prevSemIdx = -1;
                    
                    aqcuireSem(sensorSeq, this.trainDir);

                    for (int i = 0; i < semaphoresArr.length; i++) 
                        System.out.println("Train:" + this.trainID + " Sensor" + sensorSeq + " Semaphore " + i + ": "+ semaphoresArr[i].availablePermits());
                } catch (CommandException e) {
                    e.printStackTrace();    // or only e.getMessage() for the error
                } catch (InterruptedException ex) {
                    ex.printStackTrace();    // or only e.getMessage() for the error
                }

            }
        }

        private void stop() throws CommandException, InterruptedException { //DONE(Ergi)
            tsi.setSpeed(this.trainID, 0);
        }

        private void turnAround() throws CommandException, InterruptedException {  
            if (this.trainDir == DIRECTION_UP) {
                this.trainDir = DIRECTION_DOWN;
            } else {
                this.trainDir = DIRECTION_UP;
            }
            Thread.sleep(1000 +(20 * Math.abs(this.trainSpeed)));
            this.setSpeed(-1 * this.trainSpeed);
        }



        private void aqcuireSem(int sensorSeq, int dir) throws CommandException, InterruptedException {
            Integer semIdx = Rail.getNextSemaphoreID(sensorSeq, dir);
            boolean notForkPath = true;
            Integer semForkIdx = -1;
            //update the previous semaphore index only if the current semaphore is acquired, otherwise the previous semaphore index will be lost
            if (this.currSemIdx!=-1) 
                this.prevSemIdx = this.currSemIdx;
            if (semIdx == -1 || semIdx == null) {
                this.currSemIdx = -1;
                return;
            }
            Semaphore sem = semaphoresArr[semIdx];
            //try to acquire the semaphore, if it's not available, block the train and wait until it's available
            if (!sem.tryAcquire()) {
                do {
                    if (IsForkPath(sensorSeq, dir)) {
                        notForkPath = false;
                        semForkIdx = getForkSemaphoreIdx(semIdx,dir);
                        if (semForkIdx == -1)
                            throw new CommandException("Fork semaphore not found");
                        sem = semaphoresArr[semForkIdx];
                        if (sem.tryAcquire())
                            break;
                    }
                    stop();
                    sem.acquire();
                } while (false);
            }

            if (notForkPath) 
                this.currSemIdx = semIdx;
            else
                this.currSemIdx = semForkIdx;
            //if the train is at the cross section, keep the previous semaphore index, because the train will need it to release the semaphore when it's completely out of the cross section
            if (this.currSemIdx == 6 && this.trainDir == DIRECTION_DOWN) {
                this.holdForkSemIdx = this.prevSemIdx;
                this.prevSemIdx = -1;
            }
            SetSwitch(notForkPath, sensorSeq, dir);
            setSpeed(this.trainSpeed);
        }

        // Sets the switch direction based on the given parameters.
        private void SetSwitch(boolean isDefault, int sensorID, int dir) throws CommandException {
            if (isDefault) setSwitchDefault(sensorID, dir);
             else SetSwitchAlt(sensorID, dir);
        }

        private boolean IsForkPath(int sensorID, int dir) {
            return (sensorID == 13) || (sensorID == 9);
        }

        private void SetSwitchAlt(int sensorID, int dir) throws CommandException {
            if (sensorID == 13 && dir == DIRECTION_UP) {
                tsi.setSwitch(SWITCH_POINT_1_X, SWITCH_POINT_1_Y, DIRECTION_UP);
            }
            if (sensorID == 13 && dir == DIRECTION_DOWN) {
                tsi.setSwitch(SWITCH_POINT_2_X, SWITCH_POINT_2_Y, DIRECTION_UP);
            }
            if (sensorID == 9 && dir == DIRECTION_UP) {
                tsi.setSwitch(SWITCH_POINT_3_X, SWITCH_POINT_3_Y, DIRECTION_UP);
            }
            if (sensorID == 9 && dir == DIRECTION_DOWN) {
                tsi.setSwitch(SWITCH_POINT_4_X, SWITCH_POINT_4_Y, DIRECTION_UP);
            }   
        }

        private void setSwitchDefault(int sensorID, int dir) throws CommandException {
            if (sensorID == 12 && dir == DIRECTION_UP) {
                tsi.setSwitch(SWITCH_POINT_2_X, SWITCH_POINT_2_Y, DIRECTION_UP);
            }
            if (sensorID == 13 && dir == DIRECTION_UP) {
                tsi.setSwitch(SWITCH_POINT_1_X, SWITCH_POINT_1_Y, DIRECTION_DOWN);
            }
            if (sensorID == 13 && dir == DIRECTION_DOWN) {
                tsi.setSwitch(SWITCH_POINT_2_X, SWITCH_POINT_2_Y, DIRECTION_DOWN);
            }
            if (sensorID == 18 && dir == DIRECTION_DOWN) {
                tsi.setSwitch(SWITCH_POINT_3_X, SWITCH_POINT_3_Y, DIRECTION_UP);
            }
            if (sensorID == 11 && dir == DIRECTION_DOWN) {
                tsi.setSwitch(SWITCH_POINT_3_X, SWITCH_POINT_3_Y, DIRECTION_DOWN);
            }
            if (sensorID == 9 && dir == DIRECTION_UP) {
                tsi.setSwitch(SWITCH_POINT_3_X, SWITCH_POINT_3_Y, DIRECTION_DOWN);
            }
            if (sensorID == 8 && dir == DIRECTION_UP) {
                tsi.setSwitch(SWITCH_POINT_4_X, SWITCH_POINT_4_Y, DIRECTION_DOWN);
            }
            if (sensorID == 10 && dir == DIRECTION_UP) {
                tsi.setSwitch(SWITCH_POINT_4_X, SWITCH_POINT_4_Y, DIRECTION_UP);
            }
            if (sensorID == 17 && dir == DIRECTION_DOWN) {
                tsi.setSwitch(SWITCH_POINT_1_X, SWITCH_POINT_1_Y, DIRECTION_UP);
            }
            if (sensorID == 15 && dir == DIRECTION_DOWN) {
                tsi.setSwitch(SWITCH_POINT_1_X, SWITCH_POINT_1_Y, DIRECTION_DOWN);
            }
            if (sensorID == 16 && dir == DIRECTION_DOWN) {
                tsi.setSwitch(SWITCH_POINT_1_X, SWITCH_POINT_1_Y, DIRECTION_UP);
            }
            if (sensorID == 14 && dir == DIRECTION_DOWN) {
                tsi.setSwitch(SWITCH_POINT_1_X, SWITCH_POINT_1_Y, DIRECTION_DOWN);
            }
            if (sensorID == 9 && dir == DIRECTION_DOWN) {
                tsi.setSwitch(SWITCH_POINT_4_X, SWITCH_POINT_4_Y, DIRECTION_DOWN);
            }
        }

        //This method releases the last semaphore by sensor id and dirction of the train.
        public int ReleaseSemaphore(int sensorSeq, int dir, Integer semIdx) {

            if (sensorSeq == 9 && this.holdForkSemIdx != -1 && this.trainDir == DIRECTION_UP) { //release the semaphore for the fork sensor and return, because there's no previous semaphore to release
                System.out.println("Release holdForkSemIdx: " + this.holdForkSemIdx);
                semaphoresArr[this.holdForkSemIdx].release();
                this.holdForkSemIdx = -1;
                return 0;
            }

            if (semIdx == -1) {
                return -1;
            }
            //release the semaphore occupied by the train at stations
            if ((sensorSeq == 14 && dir == DIRECTION_DOWN) || (sensorSeq == 16 && dir == DIRECTION_DOWN)
            || sensorSeq == 15 || sensorSeq == 17 || sensorSeq == 1 || sensorSeq == 5) {
                return -1;
            }
            //dont release the semaphore for the fork sensor, release it when the train is completely out of the fork
            if ((semIdx == 4 || semIdx == 5) && this.trainDir == DIRECTION_DOWN) {
                return -1;
            }
            semaphoresArr[semIdx].release();
            return 0;
        }
    }

    public class Rail {
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

        public static Integer getNextSemaphoreID(int sensorIdx, int dir) {
            Integer semaphoreID = semaphoreAcquireMap.get(new AbstractMap.SimpleEntry<>(sensorIdx, dir));
            if (semaphoreID == null) {
                System.out.println("Semaphore not found for sensorIdx: " + sensorIdx + " Direction: " + dir);
                return -1;
            }
            return semaphoreID;
        }
    }
}
