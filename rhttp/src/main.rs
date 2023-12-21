#![feature(absolute_path)]
use std::{convert::Infallible, path::Path};

use axum::{
    body::Body,
    extract::State,
    http::{Request, Response, StatusCode},
    routing::{get, post},
    BoxError, Router,
};
use clap::Parser;
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
    let serve_dir = ServeDir::new(opt.directory.clone())
        .fallback(service_fn(move |req| serve_not_found(opt_m.clone(), req)));
    let app = Router::new()
        .route("/_/upload", get(upload_get))
        .route("/_/upload", post(upload_post))
        .nest_service("/", serve_dir)
        .with_state(opt)
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

async fn serve_not_found(opt: Opts, req: Request<Body>) -> Result<Response<Body>, Infallible> {
    match serve_not_found_helper(opt, req).await {
        Ok(resp) => Ok(resp),
        Err(e) => {
            log::error!("{:?}", e);
            Ok(Response::builder()
                .status(StatusCode::NOT_FOUND)
                .body(Body::from(""))
                .unwrap())
        }
    }
}

async fn serve_not_found_helper(opt: Opts, req: Request<Body>) -> Result<Response<Body>, BoxError> {
    let (_, path) = req.uri().path().split_at(1);
    let full_path = std::path::absolute(&opt.directory)?.join(path);
    log::debug!("{:?}", full_path);
    let meta = tokio::fs::metadata(&full_path).await?;
    if !meta.is_dir() {
        return Ok(Response::builder()
            .status(StatusCode::NOT_FOUND)
            .body(Body::from(""))
            .unwrap());
    }
    // list files
    let mut body = String::new();
    body.push_str(r#"<html><head><meta charset="UTF-8"/></head><body><ul>"#);
    let mut read_dir = tokio::fs::read_dir(&full_path).await?;
    while let Some(entry) = read_dir.next_entry().await? {
        let path = entry.path();
        let path = path.strip_prefix(full_path.as_path())?.to_string_lossy();
        body.push_str(&format!("<li><a href=\"{}\">{}</a></li>", path, path));
    }
    body.push_str("</ul></body></html>");

    return Ok(Response::new(Body::from(body)));
}
