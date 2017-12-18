using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace ConsoleApp1
{
   public class Runner
    {
        private List<string> _moves;
        private Dictionary<string, long> _dict = new Dictionary<string, long>();
        private int _pos = 0;
        private Queue<long> _values = new Queue<long>();
        private long _id;
        private long _sent;

        public Runner(long id, List<string> moves)
        {
            _id = id;
            _dict["p"] = id;
            _moves = moves;
        }

        public long Sent()
        {
            return _sent;
        }

        public void Store(List<long> values)
        {
            foreach (var value in values)
            {
                _values.Enqueue(value);
            }            
        }

        public List<long> Run()
        {
            var toSend = new List<long>();
            while (_pos < _moves.Count)
            {
                var m = Regex.Match(_moves[_pos], @"snd (\w+)");

                // send
                if (m.Success)
                {
                    if (!_dict.ContainsKey(m.Groups[1].Value))
                    {
                        _dict[m.Groups[1].Value] = 0;
                    }

                    _pos++;
                    _sent++;
                    toSend.Add(_dict[m.Groups[1].Value]);
                    continue;
                }

                //receive
                m = Regex.Match(_moves[_pos], @"rcv (\w+)");
                if (m.Success)
                {
                    if (_values.Count == 0)
                    {
                        return toSend;
                    }

                    var value = _values.Dequeue();
                    _dict[m.Groups[1].Value] = value;

                    _pos++;
                    continue;
                }

                m = Regex.Match(_moves[_pos], @"set (\w+) ([-\d]+)");
                if (m.Success)
                {
                    _dict[m.Groups[1].Value] = long.Parse(m.Groups[2].Value);

                    _pos++;
                    continue;
                }
                m = Regex.Match(_moves[_pos], @"set (\w+) (\w+)");
                if (m.Success)
                {
                    if (!_dict.ContainsKey(m.Groups[2].Value))
                    {
                        _dict[m.Groups[2].Value] = 0;
                    }
                    _dict[m.Groups[1].Value] = _dict[m.Groups[2].Value];

                    _pos++;
                    continue;
                }
                m = Regex.Match(_moves[_pos], @"add (\w+) ([-\d]+)");
                if (m.Success)
                {
                    if (!_dict.ContainsKey(m.Groups[1].Value))
                    {
                        _dict[m.Groups[1].Value] = 0;
                    }
                    _dict[m.Groups[1].Value] += long.Parse(m.Groups[2].Value);

                    _pos++;
                    continue;
                }
                m = Regex.Match(_moves[_pos], @"add (\w+) (\w+)");
                if (m.Success)
                {
                    if (!_dict.ContainsKey(m.Groups[1].Value))
                    {
                        _dict[m.Groups[1].Value] = 0;
                    }
                    if (!_dict.ContainsKey(m.Groups[2].Value))
                    {
                        _dict[m.Groups[2].Value] = 0;
                    }
                    _dict[m.Groups[1].Value] += _dict[m.Groups[2].Value];

                    _pos++;
                    continue;
                }
                m = Regex.Match(_moves[_pos], @"mul (\w+) ([-\d]+)");
                if (m.Success)
                {
                    if (!_dict.ContainsKey(m.Groups[1].Value))
                    {
                        _dict[m.Groups[1].Value] = 0;
                    }
                    _dict[m.Groups[1].Value] *= long.Parse(m.Groups[2].Value);

                    _pos++;
                    continue;
                }
                m = Regex.Match(_moves[_pos], @"mul (\w+) (\w+)");
                if (m.Success)
                {
                    if (!_dict.ContainsKey(m.Groups[1].Value))
                    {
                        _dict[m.Groups[1].Value] = 0;
                    }
                    if (!_dict.ContainsKey(m.Groups[2].Value))
                    {
                        _dict[m.Groups[2].Value] = 0;
                    }
                    _dict[m.Groups[1].Value] *= _dict[m.Groups[2].Value];

                    _pos++;
                    continue;
                }
                m = Regex.Match(_moves[_pos], @"mod (\w+) ([-\d]+)");
                if (m.Success)
                {
                    if (!_dict.ContainsKey(m.Groups[1].Value))
                    {
                        _dict[m.Groups[1].Value] = 0;
                    }
                    _dict[m.Groups[1].Value] = _dict[m.Groups[1].Value] % long.Parse(m.Groups[2].Value);

                    _pos++;
                    continue;
                }
                m = Regex.Match(_moves[_pos], @"mod (\w+) (\w+)");
                if (m.Success)
                {
                    if (!_dict.ContainsKey(m.Groups[1].Value))
                    {
                        _dict[m.Groups[1].Value] = 0;
                    }
                    _dict[m.Groups[1].Value] = _dict[m.Groups[1].Value] % _dict[m.Groups[2].Value];

                    _pos++;
                    continue;
                }

                m = Regex.Match(_moves[_pos], @"jgz (\w+) ([-\d]+)");
                if (m.Success)
                {
                    if (!_dict.ContainsKey(m.Groups[1].Value))
                    {
                        _dict[m.Groups[1].Value] = 0;
                    }
                    if (_dict[m.Groups[1].Value] > 0)
                    {
                        _pos += (int) long.Parse(m.Groups[2].Value);
                    }
                    else
                    {
                        _pos++;
                    }
                    continue;
                }
                m = Regex.Match(_moves[_pos], @"jgz (\w+) (\w+)");
                if (m.Success)
                {
                    if (!_dict.ContainsKey(m.Groups[1].Value))
                    {
                        _dict[m.Groups[1].Value] = 0;
                    }
                    if (!_dict.ContainsKey(m.Groups[2].Value))
                    {
                        _dict[m.Groups[2].Value] = 0;
                    }
                    if (_dict[m.Groups[1].Value] > 0)
                    {
                        _pos += (int) _dict[m.Groups[2].Value];
                    }
                    else
                    {
                        _pos++;
                    }
                    continue;
                }
                throw new Exception($"Invalid move {_moves[_pos]}");
            }
            throw new Exception($"Runner {_id} has finished");
        }
    }
}