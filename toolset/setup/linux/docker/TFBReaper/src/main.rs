extern crate prctl;
extern crate nix;
extern crate libc;
extern crate procinfo;

use std::process;
use std::process::Command;
use std::env;
use std::fs;

use nix::sys::signal;

use libc::pause;

/**
 * Recurssively finds all descendant PIDs of the given PID.
 */
fn find_descendants(pid: i32) -> Vec<i32> {
  let mut pids: Vec<i32> = vec![];

  for path in fs::read_dir("/proc").unwrap() {
    let filename = path.unwrap().file_name().into_string().ok().unwrap();
    let apid = match filename.parse::<i32>() {
      Ok(apid) => apid,
      Err(_) => -1
    };
    
    if apid > 0 {
      let stat = procinfo::pid::stat(apid).unwrap();
      if stat.ppid == pid {

        pids.push(stat.ppid);
        pids.append(&mut find_descendants(stat.pid));
      }
    }
  }

  return pids;
}

/**
 * Reap will kill all descendants of this process.
 */
extern fn reap(_:i32) {
  unsafe {
    let mut done = false;
    while !done {
      let pids: Vec<i32> = find_descendants(libc::getpid());

      if pids.len() == 0 {
        done = true;
      }

      for pid in pids {
        libc::kill(pid, libc::SIGKILL);
      }
    }
  }

  process::exit(0);
}

/**
 * main
 */
fn main() {
  // Interrupt SIGTERM and SIGINT and pass to our handler.
  let sig_action = signal::SigAction::new(
    signal::SigHandler::Handler(reap),
    signal::SaFlags::empty(),
    signal::SigSet::empty());
  unsafe {
    signal::sigaction(signal::SIGINT, &sig_action).unwrap();
    signal::sigaction(signal::SIGTERM, &sig_action).unwrap();
  }

  // Here is the magic. This sets any child processes to
  // use THIS process as a 'subreaper'. What that means is
  // even if the process uses the fork-exit technicque for
  // running a daemon (which normally orphans the process
  // and causes init(1) to adopt it, which is problematic
  // for TFB because we cannot then generally kill the
  // process since it has lost all context available to us)
  // the child process will have the parent id of THIS
  // process, allowing us to kill all the processes started
  // by the suite in this way generally.
  //
  // See: http://man7.org/linux/man-pages/man2/prctl.2.html
  prctl::set_child_subreaper(true).unwrap();

  // Gather the command line arguments for the pass-through.
  let args: Vec<_> = env::args().collect();
  if args.len() > 1 {
    // This invokes whatever was passed as arguments to TFBReaper
    // on the system. This program is merely a pass-through to
    // a shell with the subreaper stuff enabled.
    let status = Command::new(&args[1])
            .args(&args[2..])
            .status()
            .expect("Failed to execute");

    // We need to wait forever; the suite will clean this 
    // process up later.
    if status.success() {
      loop {
        unsafe {
          // Pause to keep us from spiking CPU; whenever a signal
          // occurs (except SIGTERM etc which will kill this process)
          // just iterate and pause again.
          pause();
        }
      }
    }

    // If the scripts failed, we should return that code.
    process::exit(status.code().unwrap());
  }
}