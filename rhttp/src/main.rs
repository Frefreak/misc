#![feature(absolute_path)]
use std::{convert::Infallible, path::Path, time::SystemTime};

use axum::{
    body::Body,
    extract::State,
    http::{Request, Response, StatusCode},
    routing::{get, post},
    BoxError, Router,
};
use chrono::{DateTime, Local};
use clap::Parser;
use minijinja::{context, Environment};
use serde::Serialize;
use tower::service_fn;
use tower_http::{services::ServeDir, trace::TraceLayer};
use tracing_subscriber::EnvFilter;

#[derive(Parser, Clone)]
struct Opts {
    /// root directory to serve
    #[clap(short, long, aliases=&["dir"], default_value=".")]
    directory: String,
}

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .init();
    let opt = Opts::parse();
    let opt_m = opt.clone();
    let mut jinja_env = Environment::new();
    jinja_env
        .add_template("listing.html", include_str!("../files/listing.html"))
        .unwrap();
    let jinja_env_m = jinja_env.clone();
    let serve_dir = ServeDir::new(opt.directory.clone()).fallback(service_fn(move |req| {
        serve_not_found(opt_m.clone(), jinja_env_m.clone(), req)
    }));
    let app = Router::new()
        .route("/_/upload", get(upload_get))
        .route("/_/upload", post(upload_post))
        .nest_service("/", serve_dir)
        .with_state(opt)
        .with_state(jinja_env)
        .layer(TraceLayer::new_for_http());

    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
    axum::serve(listener, app).await.unwrap();
}

async fn upload_get(State(opts): State<Opts>) -> &'static str {
    todo!()
}
async fn upload_post(State(opts): State<Opts>) -> &'static str {
    todo!()
}

async fn serve_not_found<'a>(
    opt: Opts,
    jinja_env: Environment<'a>,
    req: Request<Body>,
) -> Result<Response<Body>, Infallible> {
    let uri = req.uri().to_string();
    match serve_not_found_helper(opt, jinja_env, req).await {
        Ok(resp) => Ok(resp),
        Err(e) => {
            log::debug!("{}: {:?}", uri, e);
            Ok(Response::builder()
                .status(StatusCode::NOT_FOUND)
                .body(Body::from("not found"))
                .unwrap())
        }
    }
}

#[derive(Debug, Serialize)]
struct FileStat {
    name: String,
    modified: String,
    created: String,
    size: String,
    is_dir: bool,
    idx: usize,
}

async fn serve_not_found_helper<'a>(
    opt: Opts,
    jinja_env: Environment<'a>,
    req: Request<Body>,
) -> Result<Response<Body>, BoxError> {
    let (_, path) = req.uri().path().split_at(1);
    let full_path = std::path::absolute(&opt.directory)?.join(path);
    let meta = tokio::fs::metadata(&full_path).await?;
    if !meta.is_dir() {
        return Ok(Response::builder()
            .status(StatusCode::NOT_FOUND)
            .body(Body::from("not found"))?);
    }
    // list files
    let mut files = Vec::new();
    let mut read_dir = tokio::fs::read_dir(&full_path).await?;
    while let Some(entry) = read_dir.next_entry().await? {
        let path = entry.path();
        let path = path.strip_prefix(full_path.as_path())?.to_string_lossy();
        let metadata = entry.metadata().await?;
        let is_dir = metadata.is_dir();
        let modified = metadata.modified()?;
        let created = metadata.created()?;
        let modified_dt: DateTime<Local> = modified.into();
        let created_dt: DateTime<Local> = created.into();
        let file_size = metadata.len();
        files.push(FileStat {
            name: path.to_string(),
            modified: modified_dt.format("%Y-%m-%d %T").to_string(),
            created: created_dt.format("%Y-%m-%d %T").to_string(),
            size: bytes_to_human_readable(file_size),
            is_dir,
            idx: 0,
        });
    }
    files.sort_by(|a, b| b.is_dir.cmp(&a.is_dir).then(a.name.cmp(&b.name)));
    for (idx, file) in files.iter_mut().enumerate() {
        file.idx = idx;
    }
    println!("{:?}", files);
    let template = jinja_env.get_template("listing.html")?;
    let html = template
        // .render(context! { files => files, css => include_str!("../files/bulma.min.css") })?;
        .render(context! { files => files, css => include_str!("../files/style.css") })?;
    return Ok(Response::new(Body::from(html)));
}

fn bytes_to_human_readable(bytes: u64) -> String {
    let units = ["B", "KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB"];
    if bytes == 0 {
        return "0 B".to_string();
    }
    let i = (bytes as f64).log2() / 10.0;
    let i = i.floor() as usize;
    let bytes = bytes as f64 / 1024_f64.powi(i as i32);
    format!("{:.1} {}", bytes, units[i])
}
