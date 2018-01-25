extern crate prctl;
extern crate nix;

use std::io::{self, BufRead};
use std::process::Command;

fn main() {
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
  prctl::set_child_subreaper(true);
  
  let stdin = io::stdin();

  for line in stdin.lock().lines() {
    let str = &mut line.unwrap();
    let tokens: Vec<&str> = str.split(' ').collect();

    let first_token = tokens[0];

    match first_token {
      "run" => run_command(&tokens[1..]),
      _ => (),
    }
  }
}

fn run_command(command_arr: &[&str]) {
  Command::new(&command_arr[0])
          .args(&command_arr[1..])
          .spawn()
          .unwrap_or_else(|e| {
              panic!("failed to execute process: {}", e)
          });
}