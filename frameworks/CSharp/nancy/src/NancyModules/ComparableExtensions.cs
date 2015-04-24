using System;

namespace NancyModules
{
    public static class ComparableExtensions
    {
        public static T Clamp<T>(this T value, T min, T max) where T : IComparable<T>
        {
            return value.CompareTo(min) < 0 ? min : (value.CompareTo(max) > 0 ? max : value);
        }
    }
}