// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

namespace Benchmarks.Configuration;

public class ConsoleHostScenariosConfiguration : IScenariosConfiguration
{
    private readonly string[] _args;

    public ConsoleHostScenariosConfiguration(ConsoleArgs args)
    {
        _args = args.Args;
    }

    public void ConfigureScenarios(Scenarios scenarios)
    {
        var scenarioConfig = new ConfigurationBuilder()
            .AddJsonFile("scenarios.json", optional: true)
            .AddCommandLine(_args)
            .Build();

        var enabledCount = 0;
        var configuredScenarios = scenarioConfig["scenarios"];
        if (!string.IsNullOrWhiteSpace(configuredScenarios))
        {
            Console.WriteLine("Scenario configuration found in scenarios.json and/or command line args");
            var choices = configuredScenarios.Split(',');
            foreach (var choice in choices)
            {
                enabledCount += scenarios.Enable(choice);
            }
        }
        else
        {
            Console.WriteLine("Which scenarios would you like to enable?:");
            Console.WriteLine();
            foreach (var scenario in Scenarios.GetNames())
            {
                Console.WriteLine("  " + scenario);
            }
            Console.WriteLine();
            Console.WriteLine("Type full or partial scenario names separated by commas and hit [Enter]");
            Console.Write("> ");

            var choices = Console.ReadLine().Split(new[] { ',' }, StringSplitOptions.RemoveEmptyEntries);

            if (choices.Length > 0)
            {
                foreach (var choice in choices)
                {
                    enabledCount += scenarios.Enable(choice);
                }
            }
        }

        if (enabledCount == 0)
        {
            Console.WriteLine();
            Console.WriteLine("No matching scenarios found, enabling defaults");
            scenarios.EnableDefault();
        }

        PrintEnabledScenarios(scenarios.GetEnabled());
    }

    private static void PrintEnabledScenarios(IEnumerable<EnabledScenario> scenarios)
    {
        Console.WriteLine();
        Console.WriteLine("The following scenarios were enabled:");

        var maxNameLength = scenarios.Max(s => s.Name.Length);

        foreach (var scenario in scenarios)
        {
            Console.WriteLine($"  {scenario.Name.PadRight(maxNameLength)} -> {string.Join($"{Environment.NewLine}{"".PadLeft(maxNameLength + 6)}", scenario.Paths)}");
        }
        Console.WriteLine();
    }
}
