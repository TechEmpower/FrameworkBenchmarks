// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Linq;
using Microsoft.Extensions.DependencyInjection;

namespace Benchmarks.Data
{
    public class ApplicationDbSeeder
    {
        private readonly object _locker = new object();
        private readonly IRandom _random;
        private readonly IServiceScopeFactory _serviceScopeFactory;
        private bool _seeded = false;

        public ApplicationDbSeeder(IRandom random, IServiceScopeFactory serviceScopeFactory)
        {
            _random = random;
            _serviceScopeFactory = serviceScopeFactory;
        }

        public bool Seed()
        {
            if (!_seeded)
            {
                lock (_locker)
                {
                    if (!_seeded)
                    {
                        try
                        {
                            using (var serviceScope = _serviceScopeFactory.CreateScope())
                            {
                                var dbContext = serviceScope.ServiceProvider.GetRequiredService<ApplicationDbContext>();

                                dbContext.Database.EnsureCreated();

                                var world = dbContext.World.Count();
                                var fortune = dbContext.Fortune.Count();

                                if (world == 0 || fortune == 0)
                                {
                                    if (world == 0)
                                    {
                                        for (int i = 0; i < 10000; i++)
                                        {
                                            dbContext.World.Add(new World { RandomNumber = _random.Next(1, 10001) });
                                        }
                                        dbContext.SaveChanges();
                                    }

                                    if (fortune == 0)
                                    {
                                        dbContext.Fortune.Add(new Fortune { Message = "fortune: No such file or directory" });
                                        dbContext.Fortune.Add(new Fortune { Message = "A computer scientist is someone who fixes things that aren't broken." });
                                        dbContext.Fortune.Add(new Fortune { Message = "After enough decimal places, nobody gives a damn." });
                                        dbContext.Fortune.Add(new Fortune { Message = "A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1" });
                                        dbContext.Fortune.Add(new Fortune { Message = "A computer program does what you tell it to do, not what you want it to do." });
                                        dbContext.Fortune.Add(new Fortune { Message = "Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen" });
                                        dbContext.Fortune.Add(new Fortune { Message = "Any program that runs right is obsolete." });
                                        dbContext.Fortune.Add(new Fortune { Message = "A list is only as strong as its weakest link. — Donald Knuth" });
                                        dbContext.Fortune.Add(new Fortune { Message = "Feature: A bug with seniority." });
                                        dbContext.Fortune.Add(new Fortune { Message = "Computers make very fast, very accurate mistakes." });
                                        dbContext.Fortune.Add(new Fortune { Message = "<script>alert(\"This should not be displayed in a browser alert box.\");</script>" });
                                        dbContext.Fortune.Add(new Fortune { Message = "フレームワークのベンチマーク" });

                                        dbContext.SaveChanges();
                                    }

                                    Console.WriteLine("Database successfully seeded!");
                                }
                                else
                                {
                                    Console.WriteLine("Database already seeded!");
                                }

                                _seeded = true;
                                return true;
                            }
                        }
                        catch (Exception ex)
                        {
                            Console.Error.WriteLine("Error trying to seed the database");
                            Console.Error.WriteLine(ex);

                            return false;
                        }
                    }
                }
            }

            Console.WriteLine("Database already seeded!");
            return true;
        }
    }
}