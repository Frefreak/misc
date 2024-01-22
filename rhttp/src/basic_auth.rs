use axum::{
    extract::{Request, State},
    middleware::Next,
    response::Response,
};
use axum_auth::AuthBasic;

#[derive(Clone)]
pub struct BasicAuth {
    user: String,
    pass: String,
}


impl BasicAuth {
    pub fn new(user: String, pass: String) -> BasicAuth {
        return BasicAuth {user, pass};
    }
}

pub async fn authenticator(
    AuthBasic((username, password)): AuthBasic,
    State(auth): State<BasicAuth>,
    request: Request,
    next: Next,
) -> Response {
    if let Some(password) = password {
        if username == auth.user && password == auth.pass {
            return next.run(request).await;
        }
    }
    return Response::builder()
        .status(401)
        .header("WWW-Authenticate", "Basic")
        .body("Unauthorized".into())
        .unwrap();
}
