package Setup;

class CPrintHelloworld implements Runnable
{
    public void run()
        {
            System.out.println("hello world");
        }
}

class HelloWorld
{
  public static void main(String args[])
  {
    CPrintHelloworld a = new CPrintHelloworld();
    Thread t = new Thread(a);
    t.start();
  }
}