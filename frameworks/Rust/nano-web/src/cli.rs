use crate::init_logging;
use anyhow::Result;
use clap::{CommandFactory, Parser, Subcommand};
use std::path::PathBuf;

const DEFAULT_PORT: u16 = 3000;
const VERSION: &str = include_str!("../VERSION");

#[derive(Parser)]
#[command(name = "nano-web")]
#[command(about = "Static file server built with Rust")]
#[command(
    long_about = "Static file server built with Rust\nRepository: https://github.com/radiosilence/nano-web"
)]
#[command(version = VERSION)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Option<Commands>,

    #[arg(long = "dir", default_value = "public")]
    #[arg(help = "Directory to serve")]
    pub dir: PathBuf,

    #[arg(short = 'p', long = "port", default_value_t = DEFAULT_PORT)]
    #[arg(help = "Port to listen on")]
    pub port: u16,

    #[arg(short = 'd', long = "dev")]
    #[arg(help = "Check/reload files if modified")]
    pub dev: bool,

    #[arg(long = "spa")]
    #[arg(help = "Enable SPA mode (serve index.html for all routes)")]
    pub spa: bool,

    #[arg(long = "config-prefix", default_value = "VITE_")]
    #[arg(help = "Environment variable prefix for config injection")]
    pub config_prefix: String,

    #[arg(long = "log-level", default_value = "info")]
    #[arg(help = "Log level (debug, info, warn, error)")]
    pub log_level: String,

    #[arg(long = "log-format", default_value = "console")]
    #[arg(help = "Log format (json, console)")]
    pub log_format: String,

    #[arg(long = "log-requests")]
    #[arg(help = "Log HTTP requests")]
    pub log_requests: bool,
}

#[derive(Subcommand)]
pub enum Commands {
    #[command(about = "Start the web server")]
    Serve {
        #[arg(help = "Directory to serve")]
        directory: Option<PathBuf>,

        #[arg(short = 'p', long = "port")]
        #[arg(help = "Port to listen on")]
        port: Option<u16>,

        #[arg(short = 'd', long = "dev")]
        #[arg(help = "Check/reload files if modified")]
        dev: bool,

        #[arg(long = "spa")]
        #[arg(help = "Enable SPA mode (serve index.html for all routes)")]
        spa: bool,

        #[arg(long = "config-prefix")]
        #[arg(help = "Environment variable prefix for config injection")]
        config_prefix: Option<String>,

        #[arg(long = "log-level")]
        #[arg(help = "Log level (debug, info, warn, error)")]
        log_level: Option<String>,

        #[arg(long = "log-format")]
        #[arg(help = "Log format (json, console)")]
        log_format: Option<String>,

        #[arg(long = "log-requests")]
        #[arg(help = "Log HTTP requests")]
        log_requests: bool,
    },
    #[command(about = "Show version information")]
    Version,
    #[command(about = "Generate completion script")]
    Completion {
        #[arg(value_enum)]
        shell: clap_complete::Shell,
    },
}

impl Cli {
    pub async fn run(self) -> Result<()> {
        // Initialize logging with defaults for non-serve commands
        if self.command.is_none() || !matches!(self.command, Some(Commands::Serve { .. })) {
            init_logging(&self.log_level, &self.log_format);
        }

        match self.command {
            Some(Commands::Serve {
                ref directory,
                port,
                dev,
                spa,
                config_prefix,
                log_level,
                log_format,
                log_requests,
            }) => {
                let public_dir = self.dir.clone();
                let serve_dir = directory.clone().unwrap_or(public_dir);

                // Use subcommand values or fall back to global defaults
                let final_log_level = log_level.unwrap_or(self.log_level);
                let final_log_format = log_format.unwrap_or(self.log_format);

                // Initialize logging with final values
                init_logging(&final_log_level, &final_log_format);

                let final_config = FinalServeConfig {
                    public_dir: serve_dir,
                    port: port.unwrap_or(self.port),
                    dev: dev || self.dev,
                    spa_mode: spa || self.spa,
                    config_prefix: config_prefix.unwrap_or(self.config_prefix),
                    log_requests: log_requests || self.log_requests,
                };

                final_config.serve().await
            }
            Some(Commands::Version) => {
                println!("{}", full_version());
                println!("Static file server built with Rust");
                println!("Repository: https://github.com/radiosilence/nano-web");
                Ok(())
            }
            Some(Commands::Completion { shell }) => {
                generate_completion(shell);
                Ok(())
            }
            None => {
                // Show help when no subcommand is provided
                let mut cmd = Self::command();
                cmd.print_help()?;
                Ok(())
            }
        }
    }
}

struct FinalServeConfig {
    public_dir: PathBuf,
    port: u16,
    dev: bool,
    spa_mode: bool,
    config_prefix: String,
    log_requests: bool,
}

impl FinalServeConfig {
    async fn serve(self) -> Result<()> {
        // Use Axum with our compression and caching system
        let config = crate::server::AxumServeConfig {
            public_dir: self.public_dir,
            port: self.port,
            dev: self.dev,
            spa_mode: self.spa_mode,
            config_prefix: self.config_prefix,
            log_requests: self.log_requests,
        };
        crate::server::start_axum_server(config).await
    }
}

fn full_version() -> String {
    format!("nano-web v{}", VERSION.trim())
}

fn generate_completion(shell: clap_complete::Shell) {
    use clap_complete::generate;
    use std::io;

    let mut cmd = Cli::command();
    generate(shell, &mut cmd, "nano-web", &mut io::stdout());
}
