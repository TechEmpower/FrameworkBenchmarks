using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text;

namespace Benchmarks
{
    [StructLayout(LayoutKind.Sequential, Size = 8)]
    public struct World
    {
        public int Id { get; set; }

        public int RandomNumber { get; set; }
    }
}
