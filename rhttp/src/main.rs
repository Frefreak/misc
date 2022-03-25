#![feature(async_closure)]
use clap::StructOpt;

mod cli;
mod server;

#[tokio::main]
async fn main() -> std::io::Result<()> {
    let opts = cli::Opts::parse();
    server::start_server(opts).await
}
