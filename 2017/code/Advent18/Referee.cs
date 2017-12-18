using System;
using System.Collections.Generic;

namespace ConsoleApp1
{
    public class Referee
    {
        public static void Go(List<string> moves)
        {
            var run1 = new Runner(0, moves);
            var run2 = new Runner(1, moves);
            var count = 1;
            while (true)
            {
                Console.WriteLine($"Sent by 0: {run1.Sent()}, Sent by 1: {run2.Sent()}");
                var res1 = run1.Run();
                var res2 = run2.Run();
                if (res1.Count == 0 && res2.Count == 0)
                {
                    // deadlock
                    Console.WriteLine($"Deadlock after {count}");
                    return;
                }
                run1.Store(res2);
                run2.Store(res1);
                count++;
            }
        }
    }
}