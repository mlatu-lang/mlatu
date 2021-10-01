use std::env;
use std::path::Path;
use std::process::Command;

use swipl_info::get_swipl_info;

fn main() {
  println!("cargo:rerun-if-changed=build.rs");
  println!("cargo:rerun-if-changed=mlatu.pl");
  println!("cargo:rustc-link-arg=-Wl,-rpath,{}", get_swipl_info().lib_dir);

  let out_dir = env::var_os("OUT_DIR").unwrap();
  let dest_path = Path::new(&out_dir).join("mlatu.pl.save");
  Command::new("swipl").arg("-o")
                       .arg(dest_path)
                       .arg("--goal=true")
                       .arg("-c")
                       .arg("mlatu.pl")
                       .spawn()
                       .expect("spawn")
                       .wait()
                       .expect("wait");
}
