public class HelloWorld extends Thread {
    @Override
    public void run() {
      System.out.println("Hello, World!");
    }
    public static void main(String[] args) {
      HelloWorld thread = new HelloWorld();
      thread.start();
    }
  }
  