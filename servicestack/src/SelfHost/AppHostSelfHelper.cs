using System;
using System.Linq;
using System.Net;
using System.Collections.Generic;
using System.Diagnostics;

namespace ServiceStackBenchmark
{
    public static class AppHostSelfHelper
    {
        public static bool IsMono()
        {
            return Type.GetType("Mono.Runtime") != null;
        }

        public static void StartListening(this AppSelfHost appHost, string urlBase)
        {
            var addedURLToACL = false;

            try
            {
                appHost.Start(urlBase);
            }
            catch (HttpListenerException ex)
            {
                if (IsMono())
                    throw ex;

                if (ex.ErrorCode == 5)
                {
                    AppHostSelfHelper.AddAddress(urlBase);
                    addedURLToACL = true;
                    appHost.Start(urlBase);
                }
            }

            Console.WriteLine("AppHost Created at {0}, listening on {1}", DateTime.Now, urlBase);
            Console.WriteLine("Press <Esc> to stop.");
            do { } while (Console.ReadKey(true).Key != ConsoleKey.Escape);

            if (addedURLToACL)
                AppHostSelfHelper.DeleteAddress(urlBase);
        }


        #region Methods to Handle URL Listening with ACL Permissions

        public static void AddAddress(string address)
        {
            AddAddress(address, Environment.UserDomainName, Environment.UserName);
        }

        public static void AddAddress(string address, string domain, string user)
        {
            string args = string.Format(@"http add urlacl url={0} user={1}\{2} listen=yes", address, domain, user);

            var psi = new ProcessStartInfo("netsh", args)
            {
                Verb = "runas",
                CreateNoWindow = true,
                WindowStyle = ProcessWindowStyle.Hidden,
                UseShellExecute = true
            };

            Process.Start(psi).WaitForExit();
        }

        public static void DeleteAddress(string address)
        {
            string args = string.Format(@"http delete urlacl url={0}", address);

            var psi = new ProcessStartInfo("netsh", args)
            {
                CreateNoWindow = true,
                WindowStyle = ProcessWindowStyle.Hidden,
                UseShellExecute = true
            };

            Process.Start(psi).WaitForExit();
        }


        #endregion
    }
}
