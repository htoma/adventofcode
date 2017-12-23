namespace ConsoleApp1
{
    public class Infinite
    {       
        public long GetH()
        {
            long b = 57 * 100 + 100000;
            long c = b + 17000;

            long h = 0;
            while (true)
            {
                long f = 1;
                
                while (true)
                {
                    for (int d = 2; d * d <= b; d++)
                    {
                        if (b % d == 0)
                        {
                            f = 0;
                        }
                    }

                    if (f == 0)
                    {
                        h++;
                    }
                    if (b == c)
                    {
                        return h;
                    }
                    b += 17;
                    break;
                }
            }
        }
    }
}