package hello.loom;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Field;
import java.util.concurrent.Executor;

public final class LoomSupport {
   private static final boolean SUPPORTED;
   private static Throwable FAILURE;

   private static final MethodHandle SCHEDULER;

   static {
      boolean sup;
      MethodHandle scheduler;
      try {
         // this is required to override the default scheduler
         MethodHandles.Lookup lookup = MethodHandles.lookup();
         Field schedulerField = Class.forName("java.lang.ThreadBuilders$VirtualThreadBuilder")
               .getDeclaredField("scheduler");
         schedulerField.setAccessible(true);
         scheduler = lookup.unreflectSetter(schedulerField);

         // this is to make sure we fail earlier!
         var builder = Thread.ofVirtual();
         scheduler.invoke(builder, new Executor() {
            @Override
            public void execute(Runnable command) {

            }
         });

         FAILURE = null;

         sup = true;
      } catch (Throwable e) {
         scheduler = null;
         sup = false;
         FAILURE = e;
      }

      SCHEDULER = scheduler;
      SUPPORTED = sup;
   }

   private LoomSupport() {
   }

   public static boolean isSupported() {
      return SUPPORTED;
   }

   public static void checkSupported() {
      if (!isSupported()) {
         throw new UnsupportedOperationException(FAILURE);
      }
   }


   public static Thread.Builder.OfVirtual setVirtualThreadFactoryScheduler(Thread.Builder.OfVirtual builder,
                                                                           Executor vthreadScheduler) {
      checkSupported();
      try {
         SCHEDULER.invoke(builder, vthreadScheduler);
         return builder;
      } catch (Throwable e) {
         throw new RuntimeException(e);
      }
   }
}
