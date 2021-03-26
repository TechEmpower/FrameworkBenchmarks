use crate::epoll_callback;
use crate::epoll_config::*;
use crate::epoll_const::*;
use crate::epoll_ds::*;
use crate::listener;
use crate::sys_const::*;
use sys_call::sys_call;

#[inline]
pub fn go(port: u16) {
   let num_cpu_cores = num_cpus::get();

   let (listener_fd, _, _) = listener::get_listener_fd(port);
   listener::setup_client_connection(listener_fd, 0);

   let listener_epfd = unsafe { sys_call!(SYS_EPOLL_CREATE1 as isize, 0) };

   #[cfg(feature = "faf_debug")]
   {
      if listener_epfd < 0 {
         panic!("Failed on epoll_create(...) with err: {}", std::io::Error::from_raw_os_error(-listener_epfd as i32));
      }
   }

   let mut worker_epfds: Vec<isize> = Vec::with_capacity(num_cpu_cores);

   (0..num_cpu_cores).for_each(|_| {
      let epfd = unsafe { sys_call!(SYS_EPOLL_CREATE1 as isize, 0) };
      if epfd >= 0 {
         worker_epfds.push(epfd);
      } else {
         #[cfg(feature = "faf_debug")]
         panic!("Unable to create worker epoll instance(s)");
      }
   });

   // Debug: Log fds

   #[cfg(feature = "faf_debug")]
   {
      println!("listener fd: {}", listener_fd);
      println!("listener epfd: {}", listener_epfd);
      worker_epfds.iter().enumerate().for_each(|(i, epfd)| {
         println!("worker epfd {}: {}", i, epfd);
      });
   }

   let mut ep_event_listener = epoll_event { data: epoll_data { fd: listener_fd as i32 }, events: EPOLLIN };
   let ep_event_listener_ptr = &mut ep_event_listener as *mut epoll_event as isize;
   let mut ep_events_conections: [epoll_event; MAX_EPOLL_CONN] = unsafe { std::mem::zeroed() };

   let mut ep_events: [epoll_event; MAX_EPOLL_EVENTS_RETURNED] = unsafe { std::mem::zeroed() };
   let ep_events_ptr = &mut ep_events as *mut [epoll_event; MAX_EPOLL_EVENTS_RETURNED] as isize;

   let _ret = unsafe {
      sys_call!(
         SYS_EPOLL_CTL as isize,
         listener_epfd,
         EPOLL_CTL_ADD as isize,
         listener_fd as isize,
         ep_event_listener_ptr
      )
   };

   #[cfg(feature = "faf_debug")]
   if _ret != 0 {
      panic!("Failed on epoll_ctl(...) with err: {}", std::io::Error::from_raw_os_error(-ret as i32));
   }

   (0..num_cpu_cores).for_each(|worker_index| {
      let epfd = worker_epfds[worker_index];
      std::thread::spawn(move || {
         worker(epfd);
      });
   });

   loop {
      let num_incoming_events = unsafe {
         sys_call!(
            SYS_EPOLL_WAIT as isize,
            listener_epfd,
            ep_events_ptr,
            MAX_EPOLL_EVENTS_RETURNED as isize,
            EPOLL_TIMEOUT_MILLIS
         )
      };

      if num_incoming_events == 0 {
         continue;
      }

      (0..num_incoming_events).for_each(|index| {
         let cur_event = &ep_events[index as usize];
         let cur_fd = unsafe { cur_event.data.fd } as isize;

         if cur_fd == listener_fd as isize {
            let incoming_fd = unsafe { sys_call!(SYS_ACCEPT as isize, listener_fd as isize, 0, 0) } as isize;

            if incoming_fd >= 0 {
               let core_affin = incoming_fd as usize % num_cpu_cores;
               listener::setup_client_connection(incoming_fd, core_affin as i32);
               let saved_event = &mut ep_events_conections[incoming_fd as usize];
               saved_event.data.uint64_t = 0;
               saved_event.data.fd = incoming_fd as i32;
               saved_event.events = EPOLLIN;

               let _ret = unsafe {
                  sys_call!(
                     SYS_EPOLL_CTL as isize,
                     worker_epfds[core_affin],
                     EPOLL_CTL_ADD as isize,
                     incoming_fd as isize,
                     saved_event as *const epoll_event as isize
                  )
               };

               #[cfg(feature = "faf_debug")]
               if _ret < 0 {
                  panic!("failed to add incoming fd to epoll instance");
               }
            }
         }
      });
   }
}

#[inline]
fn worker(epfd: isize) {
   let mut ep_events: [epoll_event; MAX_EPOLL_EVENTS_RETURNED] = unsafe { std::mem::zeroed() };
   let ep_events_ptr = &ep_events as *const _ as isize;
   let mut headers_buff: [faf_pico_sys::phr_header; MAX_HEADERS_TO_PARSE] = unsafe { std::mem::zeroed() };

   let mut request_buffer: [u8; REQ_RES_BUFF_SIZE] = unsafe { std::mem::zeroed() };
   let request_buffer_ptr = &mut request_buffer as *mut _ as isize;

   let mut response_buffer: [u8; REQ_RES_BUFF_SIZE] = unsafe { std::mem::zeroed() };
   let response_buffer_ptr = &mut response_buffer as *mut _ as isize;

   loop {
      let num_incoming_events = unsafe {
         sys_call!(
            SYS_EPOLL_WAIT as isize,
            epfd,
            ep_events_ptr,
            MAX_EPOLL_EVENTS_RETURNED as isize,
            EPOLL_TIMEOUT_MILLIS
         )
      };

      if num_incoming_events == 0 {
         continue;
      }

      for index in 0..num_incoming_events {
         let cur_event = &mut ep_events[index as usize];
         let cur_fd = unsafe { cur_event.data.fd } as i32;

         let len_read = unsafe { sys_call!(SYS_READ as isize, cur_fd as isize, request_buffer_ptr, 1024) };

         if -len_read == EAGAIN as isize || -len_read == EINTR as isize {
            continue;
         } else if len_read <= 0 {
            unsafe { close_connection(epfd, cur_fd as isize) };
         } else {
            let mut method: *const i8 = std::ptr::null_mut();
            let mut method_len = 0;
            let mut path: *const i8 = std::ptr::null_mut();
            let mut path_len = 0;
            let mut minor_version = 0;
            let mut headers_len = MAX_HEADERS_TO_PARSE;
            let prev_buf_len = 0;

            let ret = unsafe {
               faf_pico_sys::phr_parse_request(
                  request_buffer_ptr as *const _,
                  len_read as usize,
                  &mut method,
                  &mut method_len,
                  &mut path,
                  &mut path_len,
                  &mut minor_version,
                  headers_buff.as_mut_ptr(),
                  &mut headers_len,
                  prev_buf_len,
               )
            };

            if ret != len_read as i32 {
               unsafe { close_connection(epfd, cur_fd as isize) };
            }

            let response_buffer_filled =
               epoll_callback::cb(method, method_len, path, path_len, &headers_buff, headers_len, &mut response_buffer);

            if response_buffer_filled > 0 {
               let wrote = unsafe {
                  sys_call!(SYS_WRITE as isize, cur_fd as isize, response_buffer_ptr, response_buffer_filled as isize)
               };

               if wrote != response_buffer_filled as isize {
                  unsafe { close_connection(epfd, cur_fd as isize) };
               }
            } else {
               unsafe { close_connection(epfd, cur_fd as isize) };
            }
         }
      }
   }
}

#[inline]
unsafe fn close_connection(epfd: isize, fd: isize) {
   listener::prepare_abort_connection(fd);
   sys_call!(SYS_EPOLL_CTL as isize, epfd, EPOLL_CTL_DEL as isize, fd, 0);
   sys_call!(SYS_CLOSE as isize, fd);
}

#[inline]
unsafe fn deregister_fd(epfd: isize, fd: isize) -> isize {
   sys_call!(SYS_EPOLL_CTL as isize, epfd, EPOLL_CTL_DEL as isize, fd, 0)
}
