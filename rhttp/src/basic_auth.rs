use axum::{
    extract::{Request, State}, middleware::Next, response::Response
};
use base64::{engine::general_purpose, Engine};

#[derive(Clone)]
pub struct BasicAuth {
    user: String,
    pass: String,
}


impl BasicAuth {
    pub fn new(user: String, pass: String) -> BasicAuth {
        Self {user, pass}
    }
}

pub async fn authenticator(
    State(auth): State<BasicAuth>,
    request: Request,
    next: Next,
) -> Response {
    if let Some(auth_header) = request.headers().get("Authorization") {
        if let Ok(auth_str) = auth_header.to_str() {
            if let Some(rem) = auth_str.strip_prefix("Basic ") {
                if let Ok(decoded) = general_purpose::STANDARD.decode(rem) {
                    if let Ok(credentials) = String::from_utf8(decoded) {
                        let mut split = credentials.splitn(2, ':');
                        if let (Some(username), Some(password)) = (split.next(), split.next()) {
                            if username == auth.user && password == auth.pass {
                                return next.run(request).await;
                            }
                        }
                    }
                }
            }
        }
    }
    Response::builder()
        .status(401)
        .header("WWW-Authenticate", "Basic")
        .body("Unauthorized".into())
        .unwrap()
}
