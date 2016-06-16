// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using System;
using System.Linq;

namespace Benchmarks.Data
{
    public class ApplicationDbSeeder
    {
        private readonly object _locker = new object();
        private readonly IRandom _random;
        private readonly ApplicationDbContext _dbContext;
        private bool _seeded = false;

        public ApplicationDbSeeder(IRandom random, ApplicationDbContext dbContext)
        {
            _random = random;
            _dbContext = dbContext;
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
                            var world = _dbContext.World.Count();
                            var fortune = _dbContext.Fortune.Count();

                            if (world == 0 || fortune == 0)
                            {
                                if (world == 0)
                                {
                                    for (int i = 0; i < 10000; i++)
                                    {
                                        _dbContext.World.Add(new World { RandomNumber = _random.Next(1, 10001) });
                                    }
                                    _dbContext.SaveChanges();
                                }

                                if (fortune == 0)
                                {
                                    _dbContext.Fortune.Add(new Fortune { Message = "fortune: No such file or directory" });
                                    _dbContext.Fortune.Add(new Fortune { Message = "A computer scientist is someone who fixes things that aren't broken." });
                                    _dbContext.Fortune.Add(new Fortune { Message = "After enough decimal places, nobody gives a damn." });
                                    _dbContext.Fortune.Add(new Fortune { Message = "A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1" });
                                    _dbContext.Fortune.Add(new Fortune { Message = "A computer program does what you tell it to do, not what you want it to do." });
                                    _dbContext.Fortune.Add(new Fortune { Message = "Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen" });
                                    _dbContext.Fortune.Add(new Fortune { Message = "Any program that runs right is obsolete." });
                                    _dbContext.Fortune.Add(new Fortune { Message = "A list is only as strong as its weakest link. — Donald Knuth" });
                                    _dbContext.Fortune.Add(new Fortune { Message = "Feature: A bug with seniority." });
                                    _dbContext.Fortune.Add(new Fortune { Message = "Computers make very fast, very accurate mistakes." });
                                    _dbContext.Fortune.Add(new Fortune { Message = "<script>alert(\"This should not be displayed in a browser alert box.\");</script>" });
                                    _dbContext.Fortune.Add(new Fortune { Message = "フレームワークのベンチマーク" });

                                    _dbContext.SaveChanges();
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
                        catch (Exception ex)
                        {
                            Console.Error.WriteLine("Error trying to seed the database. Have you run 'dnx ef database update'?");
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