// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using System;
using System.Text;

namespace PlatformBenchmarks
{
   internal static class StringBuilderCache
   {
      private const int DefaultCapacity = 1386;
      private const int MaxBuilderSize = DefaultCapacity * 3;

      [ThreadStatic]
      private static StringBuilder t_cachedInstance;

      /// <summary>Get a StringBuilder for the specified capacity.</summary>
      /// <remarks>If a StringBuilder of an appropriate size is cached, it will be returned and the cache emptied.</remarks>
      public static StringBuilder Acquire(int capacity = DefaultCapacity)
      {
         if (capacity <= MaxBuilderSize)
         {
            StringBuilder sb = t_cachedInstance;
            if (capacity < DefaultCapacity)
            {
               capacity = DefaultCapacity;
            }

            if (sb != null)
            {
               // Avoid stringbuilder block fragmentation by getting a new StringBuilder
               // when the requested size is larger than the current capacity
               if (capacity <= sb.Capacity)
               {
                  t_cachedInstance = null;
                  sb.Clear();
                  return sb;
               }
            }
         }
         return new StringBuilder(capacity);
      }

      public static void Release(StringBuilder sb)
      {
         if (sb.Capacity <= MaxBuilderSize)
         {
            t_cachedInstance = sb;
         }
      }

      public static string GetStringAndRelease(StringBuilder sb)
      {
         string result = sb.ToString();
         Release(sb);
         return result;
      }
   }
}