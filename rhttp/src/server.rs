use actix_files as fs;
use actix_web::{App, HttpServer, middleware::{Logger, Condition}, error::ErrorForbidden};
use env_logger::Env;
use actix_web_httpauth::{middleware::HttpAuthentication, extractors::basic::Config};
use crate::cli;

pub async fn start_server(opts: cli::Opts) -> std::io::Result<()> {
    let dir = opts.dir.clone();
    let auth = opts.auth.unwrap_or("".into());
    env_logger::init_from_env(Env::default().default_filter_or("info"));
    HttpServer::new(move || {
        let auth = auth.clone();
        let auth2 = auth.clone();
        let basic_auth = HttpAuthentication::basic(move |req, cred| {
            let auth = auth.clone();
            async move {
                if cred.password().is_none() {
                    return Err(ErrorForbidden("wrong"));
                };
                let user_auth = format!("{}:{}", cred.user_id(), cred.password().unwrap());
                if user_auth == auth {
                    Ok(req)
                } else {
                    Err(ErrorForbidden("wrong"))
                }
            }
        });
        App::new().service(fs::Files::new("/", dir.clone()).show_files_listing())
            .app_data(Config::default().realm("Restricted area"))
            .wrap(Logger::default())
            .wrap(Condition::new(auth2 != "", basic_auth))
    })
    .bind(("0.0.0.0", opts.port))?
    .run().await
}
