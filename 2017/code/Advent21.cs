using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using codility.Code;

namespace ConsoleApp1
{
    public class Rule
    {
        public int Size { get; set; }
        public int[,] Input { get; set; }
        public int[,] Output { get; set; }
    }

    public class Art
    {
        private List<Rule> _rules = new List<Rule>();

        private void ReadInput(string file)
        {
            var content = File.ReadAllLines(file);
            foreach (var line in content)
            {
                var m = Regex.Match(line, @"([.#/]+) => ([.#/]+)");
                var input = m.Groups[1].Value;
                var output = m.Groups[2].Value;
                var rin = Parse(input);
                var rout = Parse(output);
                var rule = new Rule
                {
                    Size = rin.Item1,
                    Input = rin.Item2,
                    Output = rout.Item2
                };
                _rules.Add(rule);
            }
        }

        private Tuple<int, int[,]> Parse(string rule)
        {
            var split = rule.Split(new[] {'/'});
            var result = new int[split.Length, split.Length];
            for (int i = 0; i < split.Length; i++)
            {
                for (int j = 0; j < split[i].Length; j++)
                {
                    if (split[i][j] == '.')
                    {
                        result[i, j] = 0;
                    }
                    else
                    {
                        result[i, j] = 1;
                    }
                }
            }
            return new Tuple<int, int[,]>(split.Length, result);
        }

        public int Do(string file)
        {
            ReadInput(file);
            var size = 3;
            var step = 0;
            var current = new[,] {{0, 1, 0}, {0, 0, 1}, {1, 1, 1}};
            while (step < 5)
            {
                if (size % 2 == 0)
                {
                    var newSize = size / 2 * 3;
                    var result = new int[newSize, newSize];
                    for (int i = 0; i < size; i += 2)
                    {
                        for (int j = 0; j < size; j += 2)
                        {
                            var tmp = Process(current, i, j, 2);
                            for (int x = i / 2 * 3; x < (i / 2 + 1) * 3; x++)
                            {
                                for (int y = j / 2 * 3; y < (j / 2 + 1) * 3; y++)
                                {
                                    result[x, y] = tmp[x - (i / 2 * 3), y - j / 2 * 3];
                                }
                            }
                        }
                    }
                    size = newSize;
                    current = result;
                }
                else if (size % 3 == 0)
                {
                    var newSize = size / 3 * 4;
                    var result = new int[newSize, newSize];
                    for (int i = 0; i < size; i += 3)
                    {
                        for (int j = 0; j < size; j += 3)
                        {
                            var tmp = Process(current, i, j, 3);
                            for (int x = i / 3 * 4; x < (i / 3 + 1) * 4; x++)
                            {
                                for (int y = j / 3 * 4; y < (j / 3 + 1) * 4; y++)
                                {
                                    result[x, y] = tmp[x - (i / 3 * 4), y - j / 3 * 4];
                                }
                            }
                        }
                    }
                    size = newSize;
                    current = result;
                }
                else throw new Exception("Invalid size");
                step++;
            }
            return Count(current, size);
        }

        private int Count(int[,] current, int size)
        {
            var result = 0;
            for (int i = 0; i < size; i++)
            {
                for (int j = 0; j < size; j++)
                {
                    if (current[i, j] == 1)
                    {
                        result++;
                    }
                }
            }
            return result;
        }

        private int[,] Process(int[,] current, int iStart, int jStart, int size)
        {
            var mine = new int[size, size];
            for (int i = iStart; i < iStart + size; i++)
            {
                for (int j = jStart; j < jStart + size; j++)
                {
                    mine[i - iStart, j - jStart] = current[i, j];
                }
            }
            var result = Compare(mine, size);
            return result;
        }

        private int[,] Compare(int[,] mine, int size)
        {
            var rules = _rules.Where(x => x.Size == size).ToList();
            var found = rules.First(x => IsMatch(x, mine, size));
            return found.Output;
        }

        private bool IsMatch(Rule rule, int[,] mine, int size)
        {
            var derived = GetDerived(mine, size);
            var found = derived.FirstOrDefault(x => AreEqual(rule.Input, x, size));
            return found != null;
        }

        private List<int[,]> GetDerived(int[,] mine, int size)
        {
            var flip = Flip(mine, size);
            var result = new List<int[,]> {mine, flip};
            var r1 = Rotate(mine, size);
            var r2 = Rotate(r1, size);
            var r3 = Rotate(r2, size);
            result.Add(r1);
            result.Add(r2);
            result.Add(r3);
            r1 = Rotate(flip, size);
            r2 = Rotate(r1, size);
            r3 = Rotate(r2, size);
            result.Add(r1);
            result.Add(r2);
            result.Add(r3);
            return result;
        }

        private int[,] Flip(int[,] a, int n)
        {
            var b = new int[n, n];
            for (int i = 0; i < n; i++)
            {
                for (int j = 0; j < n / 2; j++)
                {
                    b[i, n - j - 1] = a[i, j];
                    b[i, j] = a[i, n - j - 1];
                }
                if (n % 2 == 1)
                {
                    b[i, n / 2] = a[i, n / 2];
                }
            }
            return b;
        }

        private int[,] Rotate(int[,] a, int n)
        {
            var b = new int[n, n];

            for (int i = 0; i < n; i++)
            {
                for (int j = 0; j < n; j++)
                {
                    b[j, n - i - 1] = a[i, j];
                }
            }
            return b;
        }

        private bool AreEqual(int[,] a, int[,] b, int l)
        {
            for (int i = 0; i < l; i++)
            {
                for (int j = 0; j < l; j++)
                {
                    if (a[i, j] != b[i, j])
                    {
                        return false;
                    }
                }
            }
            return true;
        }
    }
}