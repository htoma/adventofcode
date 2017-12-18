using System;
using System.Collections.Generic;

namespace ConsoleApp1
{
    public class Referee
    {
        public static void Go(List<string> moves)
        {
            var run0 = new Runner(0, moves);
            var run1 = new Runner(1, moves);
            while (true)
            {
                var res1 = run1.Run();
                var res0 = run0.Run();
                if (res0.Count == 0 && res1.Count == 0)
                {
                    // deadlock
                    Console.WriteLine($"Result {run1.Sent()}");
                    return;
                }
                run0.Store(res1);
                run1.Store(res0);
            }
        }
    }
}