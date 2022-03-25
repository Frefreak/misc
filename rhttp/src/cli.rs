use clap::Parser;

/// Simple program to greet a person
#[derive(Parser, Clone, Debug)]
#[clap(version="0.1", about="rust http static file server")]
pub struct Opts {
    /// Specify port to listen on
    #[clap(short, long, default_value="3000")]
    pub port: u16,

    /// Specify directory
    #[clap(short, long, default_value = ".")]
    pub dir: String,

    /// Basic auth in format 'abc:def'
    #[clap(short, long)]
    pub auth: Option<String>

    //TODO: support tls
}
