pub use env_logger;
pub use log;
pub use log::{error, warn, info, debug, trace};

// TODO: Doc test
pub fn init_logger(log_var: &str, style_var: &str, debug: bool) {
    let env = env_logger::Env::default()
        .filter_or(log_var, "error")
        .write_style_or(style_var, "always");

    env_logger::init_from_env(env);

    if debug {
        error!("`error`s can be seen");
        warn!("`warn`s can be seen");
        info!("`info`s can be seen");
        debug!("`debug`s can be seen");
        trace!("`trace`s can be seen");
    }
}
