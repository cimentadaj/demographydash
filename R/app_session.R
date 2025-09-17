#' Session-specific Directory Management
#'
#' Functions to handle per-session temporary directories for multi-user isolation
#' in Shiny Server deployments.

#' Get session-specific base directory
#'
#' Creates a unique directory path for the current session under /tmp/hasdaney213.
#' This ensures that concurrent users don't interfere with each other's simulations.
#'
#' @param session Shiny session object
#' @return Character string path to session-specific directory
#' @export
get_session_base_dir <- function(session) {
  file.path("/tmp/hasdaney213", session$token)
}

#' Ensure session directory exists
#'
#' Creates the session-specific directory if it doesn't exist and returns the path.
#'
#' @param session Shiny session object
#' @return Character string path to the created session directory
#' @export
ensure_session_dir <- function(session) {
  session_dir <- get_session_base_dir(session)
  if (!dir.exists(session_dir)) {
    dir.create(session_dir, recursive = TRUE, showWarnings = FALSE)
    cat("[SESSION] Created session directory:", session_dir, "\n")
  }
  session_dir
}

#' Cleanup session directory
#'
#' Removes the session directory and all its contents when the session ends.
#' This prevents disk space accumulation from temporary files.
#'
#' @param session_dir Character string path to the session directory to remove
#' @export
cleanup_session_dir <- function(session_dir) {
  if (dir.exists(session_dir)) {
    unlink(session_dir, recursive = TRUE)
    cat("[SESSION] Cleaned up session directory:", session_dir, "\n")
  }
}

#' Setup session directory management
#'
#' Convenience function to initialize session directory and register cleanup handler.
#' Call this early in your server function.
#'
#' @param session Shiny session object
#' @return Character string path to the session directory
#' @export
setup_session_dir <- function(session) {
  session_dir <- ensure_session_dir(session)

  # Register cleanup handler for when session ends
  session$onSessionEnded(function() {
    cleanup_session_dir(session_dir)
  })

  cat("[SESSION] Setup complete for session:", session$token, "at:", session_dir, "\n")
  session_dir
}