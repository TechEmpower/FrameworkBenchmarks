use std::error::Error;
use cargo_px_env::generated_pkg_manifest_path;
use pavex_cli_client::Client;
use tfb_pavex_bp::blueprint;

/// Generate the `api_server_sdk` crate using `pavex`'s CLI.
/// 
/// `pavex` will automatically wire all our routes, constructors and error handlers
/// into the a "server SDK" that can be used by the final API server binary to launch
/// the application.
fn main() -> Result<(), Box<dyn Error>> {
    let generated_dir = generated_pkg_manifest_path()?.parent().unwrap().into();
    let blueprint = blueprint();
    Client::new()
        .generate(blueprint, generated_dir)
        .execute()?;
    Ok(())
}
