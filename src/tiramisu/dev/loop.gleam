//// Render loop — owns minimal mutable state for 60fps rendering.
////
//// The RenderLoop is an opaque FFI type that runs requestAnimationFrame
//// and manages per-frame state that must be mutable for performance:
//// the active camera reference.
////
//// All other scene state lives in the functional Registry, updated through
//// Lustre's message dispatch cycle.

import savoiardi.{type Camera, type Renderer, type Scene}

// TYPE ------------------------------------------------------------------------

/// Opaque type wrapping the JS render loop closure.
/// Holds only the minimal mutable state needed for 60fps rendering.
pub type RenderLoop

// LIFECYCLE -------------------------------------------------------------------

/// Start the render loop for a scene.
/// Begins requestAnimationFrame immediately.
@external(javascript, "./render_loop.ffi.mjs", "start")
pub fn start(scene: Scene, renderer: Renderer, scene_id: String) -> RenderLoop

/// Stop the render loop (cancels requestAnimationFrame).
@external(javascript, "./render_loop.ffi.mjs", "stop")
pub fn stop(loop: RenderLoop) -> Nil

// CAMERA ----------------------------------------------------------------------

/// Set the active camera used for rendering.
/// Called when a camera is created with active=true or when active state changes.
@external(javascript, "./render_loop.ffi.mjs", "setActiveCamera")
pub fn set_active_camera(loop: RenderLoop, camera: Camera) -> Nil

/// Clear the active camera (stops rendering until a new one is set).
@external(javascript, "./render_loop.ffi.mjs", "clearActiveCamera")
pub fn clear_active_camera(loop: RenderLoop) -> Nil
