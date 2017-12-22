using System;
using System.IO;

namespace ConsoleApp1
{
    public class Virus
    {
        public enum Direction
        {
            Up = 0,
            Right = 1, 
            Down = 2, 
            Left = 3
        }

        private int _count;
        private int _posi;
        private int _posj;
        private Direction _direction;
        private const int _n = 10001;
        private char[,] _matrix = new char[_n, _n];

        public void Do(string file)
        {
            for (int i = 0; i < _n; i++)
            {
                for (int j = 0; j < _n; j++)
                {
                    _matrix[i, j] = '.';
                }
            }


            var content = File.ReadAllLines(file);
            var n = content.Length;
            var start = (_n - n) / 2;
            for (int i = start; i < start + n; i++)
            {
                for (int j = start; j < start + n; j++)
                {
                    _matrix[i, j] = content[i - start][j - start];
                }
            }

            _posi = _n / 2;
            _posj = _n / 2;

            _count = 0;
            var step = 0;
            _direction = Direction.Up;
            while (step < 10000000)
            {
                _count += Move();
                step++;
            }
            Console.WriteLine(_count);
        }

        private int Move()
        {
            var res = 0;
            if (_matrix[_posi, _posj] == '.')
            {
                //clean->weakened
                _matrix[_posi, _posj] = 'w';
                _direction = (Direction)(((int)_direction + 4 - 1) % 4);
            }
            else if (_matrix[_posi, _posj] == 'w')
            {
                //weakened->infected
                res = 1;
                _matrix[_posi, _posj] = '#';                
            }
            else if (_matrix[_posi, _posj] == '#')
            {
                //infected->flagged
                _matrix[_posi, _posj] = 'f';
                _direction = (Direction) (((int) _direction + 1) % 4);
            }
            else
            {
                //flagged->cleaned
                _matrix[_posi, _posj] = '.';
                _direction = (Direction)(((int)_direction + 2) % 4);
            }

            switch (_direction)
            {
                case Direction.Up:
                {
                    _posi--;
                    break;
                }
                case Direction.Right:
                {
                    _posj++;
                    break;
                }
                case Direction.Down:
                {
                    _posi++;
                    break;
                }
                case Direction.Left:
                {
                    _posj--;
                    break;
                }
            }

            return res;
        }
    }
}