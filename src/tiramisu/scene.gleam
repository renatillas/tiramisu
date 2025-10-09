//// Scene graph module - declarative 3D scene construction.
////
//// This module provides types and functions for building 3D scenes declaratively.
//// Scenes are composed of `SceneNode` values that describe meshes, lights, cameras, and groups.
////
//// ## Core Concepts
////
//// - **Immutability**: Scene nodes are immutable values. Updates create new nodes.
//// - **Hierarchy**: Use `Group` nodes to create parent-child relationships.
//// - **Validation**: Geometry and material constructors return `Result` to catch invalid parameters.
//// - **Performance**: Use `InstancedMesh` for many identical objects (1 draw call instead of thousands).
////
//// ## Quick Example
////
//// ```gleam
//// import tiramisu/scene
//// import tiramisu/transform
//// import gleam/option
//// import vec/vec3
////
//// pub fn view(model: Model) {
////   let assert Ok(geometry) = scene.box(width: 1.0, height: 1.0, depth: 1.0)
////   let assert Ok(material) = scene.basic_material(color: 0xff0000, transparent: False, opacity: 1.0)
////
////   [
////     scene.Mesh(
////       id: "player",
////       geometry: geometry,
////       material: material,
////       transform: transform.at(vec3.Vec3(0.0, 1.0, 0.0)),
////       physics: option.None,
////     ),
////     scene.Light(
////       id: "sun",
////       light: scene.DirectionalLight(color: 0xffffff, intensity: 1.0),
////       transform: transform.identity,
////     ),
////   ]
//// }
//// ```

import gleam/bool
import gleam/dict
import gleam/list
import gleam/option.{type Option}
import gleam/order
import gleam/set
import tiramisu/audio.{type Audio, type AudioBuffer, type AudioConfig}
import tiramisu/camera
import tiramisu/object3d.{type AnimationPlayback, type Object3D}
import tiramisu/physics.{type RigidBody}
import tiramisu/transform
import vec/vec3.{type Vec3}

/// Opaque type for Three.js textures.
///
/// Created via `asset.load_texture()` and used in materials.
pub type Texture

/// Opaque type for Three.js BufferGeometry.
///
/// Created by loading 3D models with `asset.load_stl()` or `asset.load_model()`.
pub type BufferGeometry

/// 3D geometry types supported by the engine.
///
/// Each variant represents a different primitive shape or custom geometry.
/// Use the validated constructor functions like `box()`, `sphere()`, etc.
pub opaque type Geometry {
  BoxGeometry(width: Float, height: Float, depth: Float)
  SphereGeometry(radius: Float, width_segments: Int, height_segments: Int)
  ConeGeometry(radius: Float, height: Float, segments: Int)
  PlaneGeometry(width: Float, height: Float)
  CircleGeometry(radius: Float, segments: Int)
  CylinderGeometry(
    radius_top: Float,
    radius_bottom: Float,
    height: Float,
    radial_segments: Int,
  )
  TorusGeometry(
    radius: Float,
    tube: Float,
    radial_segments: Int,
    tubular_segments: Int,
  )
  TetrahedronGeometry(radius: Float, detail: Int)
  IcosahedronGeometry(radius: Float, detail: Int)
  CustomGeometry(BufferGeometry)
}

/// Material types for rendering objects.
///
/// Materials define how surfaces appear when rendered. Different materials
/// have different performance characteristics and visual properties.
///
/// ## Performance
///
/// - `BasicMaterial`: Fastest, no lighting calculations
/// - `LambertMaterial`, `ToonMaterial`: Fast, simple lighting
/// - `PhongMaterial`: Medium, specular highlights
/// - `StandardMaterial`: Physically-based, most realistic but slower
pub opaque type Material {
  /// Unlit material (no lighting calculations). Fast and useful for flat-shaded objects.
  BasicMaterial(
    color: Int,
    map: Option(Texture),
    normal_map: Option(Texture),
    transparent: Bool,
    opacity: Float,
  )
  /// Physically-based material with metalness/roughness workflow. Most realistic.
  StandardMaterial(
    color: Int,
    map: Option(Texture),
    normal_map: Option(Texture),
    ambient_oclusion_map: Option(Texture),
    roughness_map: Option(Texture),
    metalness_map: Option(Texture),
    metalness: Float,
    roughness: Float,
  )
  /// Shiny material with specular highlights (like plastic or ceramic).
  PhongMaterial(
    color: Int,
    map: Option(Texture),
    normal_map: Option(Texture),
    ambient_oclusion_map: Option(Texture),
    shininess: Float,
  )
  /// Matte material (like cloth or wood). Non-shiny diffuse lighting.
  LambertMaterial(
    color: Int,
    map: Option(Texture),
    normal_map: Option(Texture),
    ambient_oclusion_map: Option(Texture),
  )
  /// Cartoon-style material with banded shading.
  ToonMaterial(
    color: Int,
    map: Option(Texture),
    normal_map: Option(Texture),
    ambient_oclusion_map: Option(Texture),
  )
  /// Material for rendering lines.
  LineMaterial(color: Int, linewidth: Float)
  /// Material for 2D sprites that always face the camera.
  SpriteMaterial(
    color: Int,
    map: Option(Texture),
    normal_map: Option(Texture),
    transparent: Bool,
    opacity: Float,
  )
}

pub type MaterialError {
  InvalidMaterialColor(Int)
  InvalidMaterialOpacity(Float)
  InvalidMaterialRoughness(Float)
  InvalidMaterialMetalness(Float)
  InvalidMaterialLinewidth(Float)
  InvalidMaterialShininess(Float)
}

/// Light types for illuminating the scene.
///
/// Different lights have different performance impacts and visual characteristics.
/// Most games use a combination of ambient + directional for outdoor scenes,
/// or ambient + point/spot for indoor scenes.
pub opaque type Light {
  /// Global ambient light (affects all objects equally, no direction).
  AmbientLight(intensity: Float, color: Int)
  /// Directional light like the sun (parallel rays, infinite distance).
  DirectionalLight(intensity: Float, color: Int)
  /// Point light that radiates in all directions (like a light bulb).
  PointLight(intensity: Float, color: Int, distance: Float)
  /// Cone-shaped spotlight (like a flashlight or stage light).
  SpotLight(
    intensity: Float,
    color: Int,
    distance: Float,
    angle: Float,
    penumbra: Float,
  )
  /// Hemisphere light with different colors for sky and ground (outdoor ambient).
  HemisphereLight(intensity: Float, sky_color: Int, ground_color: Int)
}

pub type LightError {
  InvalidLightIntensity(Float)
  InvalidLightColor(Int)
  InvalidLightDistance(Float)
}

pub fn ambient_light(
  intensity intensity: Float,
  color color: Int,
) -> Result(Light, LightError) {
  use <- bool.guard(intensity <. 0.0, Error(InvalidLightIntensity(intensity)))
  use <- bool.guard(
    color < 0 && color > 0xffffff,
    Error(InvalidLightColor(color)),
  )
  Ok(AmbientLight(intensity:, color:))
}

pub fn directional_light(
  intensity intensity: Float,
  color color: Int,
) -> Result(Light, LightError) {
  use <- bool.guard(intensity <. 0.0, Error(InvalidLightIntensity(intensity)))
  use <- bool.guard(
    color < 0 && color > 0xffffff,
    Error(InvalidLightColor(color)),
  )
  Ok(DirectionalLight(intensity:, color:))
}

pub fn point_light(
  intensity intensity: Float,
  color color: Int,
  distance distance: Float,
) -> Result(Light, LightError) {
  use <- bool.guard(intensity <. 0.0, Error(InvalidLightIntensity(intensity)))
  use <- bool.guard(
    color < 0 && color > 0xffffff,
    Error(InvalidLightColor(color)),
  )
  use <- bool.guard(distance <. 0.0, Error(InvalidLightDistance(distance)))
  Ok(PointLight(intensity:, color:, distance:))
}

pub fn spotlight(
  intensity intensity: Float,
  color color: Int,
  distance distance: Float,
  angle angle: Float,
  penumbra penumbra: Float,
) -> Result(Light, LightError) {
  use <- bool.guard(intensity <. 0.0, Error(InvalidLightIntensity(intensity)))
  use <- bool.guard(
    color < 0 && color > 0xffffff,
    Error(InvalidLightColor(color)),
  )
  use <- bool.guard(distance <. 0.0, Error(InvalidLightDistance(distance)))
  Ok(SpotLight(intensity:, color:, distance:, angle:, penumbra:))
}

pub fn hemisphere_light(
  intensity intensity: Float,
  sky_color sky_color: Int,
  ground_color ground_color: Int,
) -> Result(Light, LightError) {
  use <- bool.guard(intensity <. 0.0, Error(InvalidLightIntensity(intensity)))
  use <- bool.guard(
    sky_color < 0 && sky_color > 0xffffff,
    Error(InvalidLightColor(sky_color)),
  )
  use <- bool.guard(
    ground_color < 0 && ground_color > 0xffffff,
    Error(InvalidLightColor(sky_color)),
  )
  Ok(HemisphereLight(intensity:, sky_color:, ground_color:))
}

/// Level of Detail (LOD) configuration.
///
/// Defines which mesh to display based on camera distance. Use with `LOD` scene node
/// for automatic detail switching to improve performance.
///
/// ## Example
///
/// ```gleam
/// scene.LOD(
///   id: "tree",
///   levels: [
///     scene.lod_level(distance: 0.0, node: high_detail_mesh),   // 0-50 units
///     scene.lod_level(distance: 50.0, node: medium_detail_mesh), // 50-100 units
///     scene.lod_level(distance: 100.0, node: low_detail_mesh),   // 100+ units
///   ],
///   transform: transform.identity,
/// )
/// ```
pub type LODLevel {
  LODLevel(distance: Float, node: Node)
}

/// Create an LOD level with a distance threshold and scene node.
///
/// Levels should be ordered from closest (distance: 0.0) to farthest.
///
/// ## Example
///
/// ```gleam
/// let high_detail = scene.lod_level(distance: 0.0, node: detailed_mesh)
/// let low_detail = scene.lod_level(distance: 100.0, node: simple_mesh)
/// ```
pub fn lod_level(distance distance: Float, node node: Node) -> LODLevel {
  LODLevel(distance: distance, node: node)
}

/// Scene node - the core building block of your 3D scene.
///
/// Scene nodes are immutable, declarative descriptions of objects in your game.
/// Each frame, your `view()` function returns a list of scene nodes, and the engine
/// efficiently updates only what changed.
///
/// ## Node Types
///
/// - **Mesh**: Standard 3D object (1 draw call per mesh)
/// - **InstancedMesh**: Many identical objects (1 draw call total!)
/// - **Group**: Container for organizing child nodes in a hierarchy
/// - **Light**: Illuminates the scene
/// - **Camera**: Defines viewpoint (must have at least one with `active: True`)
/// - **LOD**: Switches detail levels based on distance
/// - **Model3D**: Loaded 3D model with animations
/// - **Audio**: Background or positional audio
/// - **Debug***: Visualization helpers for development
///
/// ## Example
///
/// ```gleam
/// pub fn view(model: Model) {
///   [
///     scene.Group(
///       id: "player",
///       transform: transform.at(model.position),
///       children: [
///         scene.Mesh(
///           id: "player-body",
///           geometry: scene.BoxGeometry(1.0, 2.0, 1.0),
///           material: scene.BasicMaterial(0x00ff00, False, 1.0, option.None),
///           transform: transform.identity,
///           physics: option.Some(model.physics_body),
///         ),
///       ],
///     ),
///   ]
/// }
/// ```
pub type Node {
  Mesh(
    id: String,
    geometry: Geometry,
    material: Material,
    transform: transform.Transform,
    physics: option.Option(RigidBody),
  )
  /// Instanced mesh - renders many copies of the same geometry/material with 1 draw call
  /// Much more efficient than creating individual Mesh nodes for identical objects
  InstancedMesh(
    id: String,
    geometry: Geometry,
    material: Material,
    instances: List(transform.Transform),
  )
  Group(id: String, transform: transform.Transform, children: List(Node))
  Light(id: String, light: Light, transform: transform.Transform)
  /// Camera - defines a viewpoint in the scene
  /// Only one camera can be active at a time for rendering (when viewport is None)
  /// Use effect.set_active_camera(id) to switch between cameras
  /// Set viewport to render in a specific area (for picture-in-picture effects)
  Camera(
    id: String,
    camera: camera.Camera,
    transform: transform.Transform,
    /// Optional look-at target in world space. If None, camera uses transform rotation.
    /// If Some, camera will orient itself to look at the target point.
    look_at: option.Option(vec3.Vec3(Float)),
    active: Bool,
    /// Optional viewport: (x, y, width, height) in pixels
    /// If None, camera fills entire canvas (only when active=True)
    /// If Some, camera renders in specified rectangle (regardless of active state)
    viewport: option.Option(#(Int, Int, Int, Int)),
  )
  /// Level of Detail - automatically switches between different meshes based on camera distance
  /// Levels should be ordered from closest (distance: 0.0) to farthest
  LOD(id: String, levels: List(LODLevel), transform: transform.Transform)
  Model3D(
    id: String,
    object: Object3D,
    transform: transform.Transform,
    animation: option.Option(AnimationPlayback),
    physics: option.Option(RigidBody),
  )
  Audio(id: String, buffer: AudioBuffer, config: AudioConfig, audio_type: Audio)
  // Debug visualization nodes
  DebugBox(id: String, min: Vec3(Float), max: Vec3(Float), color: Int)
  DebugSphere(id: String, center: Vec3(Float), radius: Float, color: Int)
  DebugLine(id: String, from: Vec3(Float), to: Vec3(Float), color: Int)
  DebugAxes(id: String, origin: Vec3(Float), size: Float)
  DebugGrid(id: String, size: Float, divisions: Int, color: Int)
  DebugPoint(id: String, position: Vec3(Float), size: Float, color: Int)
}

pub type GeometryError {
  InvalidGeometryWidth(width: Float)
  InvalidGeometryHeight(height: Float)
  InvalidGeometryDepth(depth: Float)
  InvalidGeometryRadius(radius: Float)
  InvalidGeometryTube(tube: Float)
  InvalidGeometrySegmentCountWidth(count: Int)
  InvalidGeometrySegmentCountHeight(count: Int)
  InvalidGeometrySegmentCount(count: Int)
}

// --- Validated Geometry Constructors ---

/// Create a validated box geometry.
///
/// All dimensions must be positive (> 0).
///
/// ## Example
///
/// ```gleam
/// let assert Ok(cube) = scene.box(width: 1.0, height: 1.0, depth: 1.0)
/// let assert Ok(wall) = scene.box(width: 10.0, height: 3.0, depth: 0.1)
/// ```
pub fn box(
  width width: Float,
  height height: Float,
  depth depth: Float,
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(width <=. 0.0, Error(InvalidGeometryWidth(width)))
  use <- bool.guard(height <=. 0.0, Error(InvalidGeometryHeight(height)))
  use <- bool.guard(depth <=. 0.0, Error(InvalidGeometryDepth(depth)))

  Ok(BoxGeometry(width, height, depth))
}

/// Create a validated sphere geometry.
///
/// Radius must be positive. Width segments >= 3, height segments >= 2.
/// More segments = smoother sphere but more triangles.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(ball) = scene.sphere(radius: 1.0, width_segments: 32, height_segments: 16)
/// let assert Ok(low_poly) = scene.sphere(radius: 1.0, width_segments: 8, height_segments: 6)
/// ```
pub fn sphere(
  radius radius: Float,
  width_segments width_segments: Int,
  height_segments height_segments: Int,
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(radius <=. 0.0, Error(InvalidGeometryRadius(radius)))
  use <- bool.guard(
    width_segments < 3,
    Error(InvalidGeometrySegmentCountWidth(width_segments)),
  )
  use <- bool.guard(
    height_segments < 2,
    Error(InvalidGeometrySegmentCountHeight(height_segments)),
  )

  Ok(SphereGeometry(radius, width_segments, height_segments))
}

pub fn cone(
  radius radius: Float,
  height height: Float,
  segments segments: Int,
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(radius <=. 0.0, Error(InvalidGeometryRadius(radius)))
  use <- bool.guard(height <=. 0.0, Error(InvalidGeometryHeight(height)))
  use <- bool.guard(segments < 3, Error(InvalidGeometrySegmentCount(segments)))

  Ok(ConeGeometry(radius, height, segments))
}

pub fn plane(
  width width: Float,
  height height: Float,
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(width <=. 0.0, Error(InvalidGeometryWidth(width)))
  use <- bool.guard(height <=. 0.0, Error(InvalidGeometryHeight(height)))

  Ok(PlaneGeometry(width, height))
}

pub fn circle(
  radius radius: Float,
  segments segments: Int,
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(radius <=. 0.0, Error(InvalidGeometryRadius(radius)))
  use <- bool.guard(segments < 3, Error(InvalidGeometrySegmentCount(segments)))

  Ok(CircleGeometry(radius, segments))
}

pub fn custom_geometry(geometry geometry: BufferGeometry) -> Geometry {
  CustomGeometry(geometry)
}

/// Create a validated cylinder geometry.
///
/// Both radii must be non-negative, height positive, radial segments >= 3.
/// Set one radius to 0 to create a cone shape.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(cylinder) = scene.cylinder(radius_top: 1.0, radius_bottom: 1.0, height: 2.0, radial_segments: 32)
/// let assert Ok(cone) = scene.cylinder(radius_top: 0.0, radius_bottom: 1.0, height: 2.0, radial_segments: 32)
/// ```
pub fn cylinder(
  radius_top radius_top: Float,
  radius_bottom radius_bottom: Float,
  height height: Float,
  radial_segments radial_segments: Int,
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(radius_top <. 0.0, Error(InvalidGeometryRadius(radius_top)))
  use <- bool.guard(
    radius_bottom <. 0.0,
    Error(InvalidGeometryRadius(radius_bottom)),
  )
  use <- bool.guard(height <=. 0.0, Error(InvalidGeometryHeight(height)))
  use <- bool.guard(
    radial_segments < 3,
    Error(InvalidGeometrySegmentCount(radial_segments)),
  )

  Ok(CylinderGeometry(radius_top, radius_bottom, height, radial_segments))
}

/// Create a validated torus (donut) geometry.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(donut) = scene.torus(radius: 2.0, tube: 0.5, radial_segments: 16, tubular_segments: 100)
/// ```
pub fn torus(
  radius radius: Float,
  tube tube: Float,
  radial_segments radial_segments: Int,
  tubular_segments tubular_segments: Int,
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(radius <=. 0.0, Error(InvalidGeometryRadius(radius)))
  use <- bool.guard(tube <=. 0.0, Error(InvalidGeometryTube(tube)))
  use <- bool.guard(
    radial_segments < 3,
    Error(InvalidGeometrySegmentCount(radial_segments)),
  )
  use <- bool.guard(
    tubular_segments < 3,
    Error(InvalidGeometrySegmentCount(tubular_segments)),
  )

  Ok(TorusGeometry(radius, tube, radial_segments, tubular_segments))
}

/// Create a validated tetrahedron (4-sided polyhedron) geometry.
///
/// Detail level controls subdivision (0 = no subdivision, higher = more triangles).
///
/// ## Example
///
/// ```gleam
/// let assert Ok(shape) = scene.tetrahedron(radius: 1.0, detail: 0)
/// ```
pub fn tetrahedron(
  radius radius: Float,
  detail detail: Int,
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(radius <=. 0.0, Error(InvalidGeometryRadius(radius)))
  use <- bool.guard(detail < 0, Error(InvalidGeometrySegmentCount(detail)))

  Ok(TetrahedronGeometry(radius, detail))
}

/// Create a validated icosahedron (20-sided polyhedron) geometry.
///
/// Detail level controls subdivision. Good for creating spheres with flat faces.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(shape) = scene.icosahedron(radius: 1.0, detail: 2)
/// ```
pub fn icosahedron(
  radius radius: Float,
  detail detail: Int,
) -> Result(Geometry, GeometryError) {
  use <- bool.guard(radius <=. 0.0, Error(InvalidGeometryRadius(radius)))
  use <- bool.guard(detail < 0, Error(InvalidGeometrySegmentCount(detail)))

  Ok(IcosahedronGeometry(radius, detail))
}

/// Create a validated basic (unlit) material.
///
/// Basic materials don't react to lights, making them very fast to render.
/// Opacity must be between 0.0 (fully transparent) and 1.0 (fully opaque).
///
/// ## Example
///
/// ```gleam
/// let assert Ok(red) = scene.basic_material(color: 0xff0000, transparent: False, opacity: 1.0)
/// let assert Ok(glass) = scene.basic_material(color: 0x88ccff, transparent: True, opacity: 0.5)
/// ```
pub fn basic_material(
  color color: Int,
  transparent transparent: Bool,
  opacity opacity: Float,
  map map: option.Option(Texture),
  normal_map normal_map: option.Option(Texture),
) -> Result(Material, MaterialError) {
  use <- bool.guard(
    color < 0x000000 || color > 0xffffff,
    Error(InvalidMaterialColor(color)),
  )
  use <- bool.guard(
    opacity <. 0.0 || opacity >. 1.0,
    Error(InvalidMaterialOpacity(opacity)),
  )

  Ok(BasicMaterial(color:, transparent:, opacity:, map:, normal_map:))
}

/// Create a validated physically-based (PBR) standard material.
///
/// Standard materials use metalness/roughness workflow for realistic rendering.
/// - Metalness: 0.0 = dielectric (plastic, wood), 1.0 = metal
/// - Roughness: 0.0 = mirror-smooth, 1.0 = completely rough
///
/// ## Example
///
/// ```gleam
/// let assert Ok(gold) = scene.standard_material(color: 0xffd700, metalness: 1.0, roughness: 0.3, map: option.None, normal_map: option.None, ao_map: option.None, roughness_map: option.None, metalness_map: option.None)
/// let assert Ok(plastic) = scene.standard_material(color: 0xff0000, metalness: 0.0, roughness: 0.5, map: option.None, normal_map: option.None, ao_map: option.None, roughness_map: option.None, metalness_map: option.None)
/// ```
pub fn standard_material(
  color color: Int,
  metalness metalness: Float,
  roughness roughness: Float,
  map map: option.Option(Texture),
  normal_map normal_map: option.Option(Texture),
  ambient_oclusion_map ambient_oclusion_map: option.Option(Texture),
  roughness_map roughness_map: option.Option(Texture),
  metalness_map metalness_map: option.Option(Texture),
) -> Result(Material, MaterialError) {
  use <- bool.guard(
    color < 0x000000 || color > 0xffffff,
    Error(InvalidMaterialColor(color)),
  )
  use <- bool.guard(
    metalness <. 0.0 || metalness >. 1.0,
    Error(InvalidMaterialMetalness(metalness)),
  )
  use <- bool.guard(
    roughness <. 0.0 || roughness >. 1.0,
    Error(InvalidMaterialRoughness(roughness)),
  )

  Ok(StandardMaterial(
    color:,
    metalness:,
    roughness:,
    map:,
    normal_map:,
    roughness_map:,
    metalness_map:,
    ambient_oclusion_map:,
  ))
}

/// Create a validated line material for rendering lines.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(line_mat) = scene.line_material(color: 0xff0000, linewidth: 2.0)
/// ```
pub fn line_material(
  color color: Int,
  linewidth linewidth: Float,
) -> Result(Material, MaterialError) {
  use <- bool.guard(
    color < 0x000000 || color > 0xffffff,
    Error(InvalidMaterialColor(color)),
  )
  use <- bool.guard(
    linewidth <=. 0.0,
    Error(InvalidMaterialLinewidth(linewidth)),
  )

  Ok(LineMaterial(color, linewidth))
}

/// Create a validated sprite material for 2D billboards.
///
/// Sprites always face the camera and are useful for particles, UI elements, etc.
///
/// ## Example
///
/// ```gleam
/// let assert Ok(sprite_mat) = scene.sprite_material(color: 0xffffff, transparent: True, opacity: 0.8)
/// ```
pub fn sprite_material(
  color color: Int,
  transparent transparent: Bool,
  opacity opacity: Float,
  map map: option.Option(Texture),
  normal_map normal_map: option.Option(Texture),
) -> Result(Material, MaterialError) {
  use <- bool.guard(
    color < 0x000000 || color > 0xffffff,
    Error(InvalidMaterialColor(color)),
  )
  use <- bool.guard(
    opacity <. 0.0 || opacity >. 1.0,
    Error(InvalidMaterialOpacity(opacity)),
  )

  Ok(SpriteMaterial(color:, transparent:, opacity:, map:, normal_map:))
}

pub fn lambert_material(
  color color: Int,
  map map: Option(Texture),
  normal_map normal_map: Option(Texture),
  ambient_oclusion_map ambient_oclusion_map: Option(Texture),
) -> Result(Material, MaterialError) {
  use <- bool.guard(
    color < 0x000000 || color > 0xffffff,
    Error(InvalidMaterialColor(color)),
  )
  Ok(LambertMaterial(color:, map:, normal_map:, ambient_oclusion_map:))
}

pub fn phong_material(
  color color: Int,
  shininess shininess: Float,
  map map: Option(Texture),
  normal_map normal_map: Option(Texture),
  ambient_oclusion_map ambient_oclusion_map: Option(Texture),
) -> Result(Material, MaterialError) {
  use <- bool.guard(
    color < 0x000000 || color > 0xffffff,
    Error(InvalidMaterialColor(color)),
  )
  use <- bool.guard(
    shininess <. 0.0,
    Error(InvalidMaterialShininess(shininess)),
  )
  Ok(PhongMaterial(color:, shininess:, map:, normal_map:, ambient_oclusion_map:))
}

pub fn toon_material(
  color color: Int,
  map map: Option(Texture),
  normal_map normal_map: Option(Texture),
  ambient_oclusion_map ambient_oclusion_map: Option(Texture),
) -> Result(Material, MaterialError) {
  use <- bool.guard(
    color < 0x000000 || color > 0xffffff,
    Error(InvalidMaterialColor(color)),
  )
  Ok(ToonMaterial(color:, map:, normal_map:, ambient_oclusion_map:))
}

// --- Material Presets ---

/// Create a plastic material (non-metallic, medium roughness).
///
/// ## Example
///
/// ```gleam
/// let red_plastic = scene.plastic(0xff0000)
/// let blue_plastic = scene.plastic(0x0000ff)
/// ```
pub fn plastic(color color: Int) -> Result(Material, MaterialError) {
  standard_material(
    color: color,
    metalness: 0.0,
    roughness: 0.5,
    map: option.None,
    normal_map: option.None,
    ambient_oclusion_map: option.None,
    roughness_map: option.None,
    metalness_map: option.None,
  )
}

/// Create a metallic material (full metalness, low roughness).
///
/// ## Example
///
/// ```gleam
/// let gold = scene.metal(0xffd700)
/// let silver = scene.metal(0xc0c0c0)
/// ```
pub fn metal(color color: Int) -> Result(Material, MaterialError) {
  standard_material(
    color: color,
    metalness: 1.0,
    roughness: 0.3,
    map: option.None,
    normal_map: option.None,
    ambient_oclusion_map: option.None,
    roughness_map: option.None,
    metalness_map: option.None,
  )
}

/// Create a rough metal material (full metalness, high roughness).
///
/// ## Example
///
/// ```gleam
/// let rusty_metal = scene.rough_metal(0x8b4513)
/// ```
pub fn rough_metal(color color: Int) -> Result(Material, MaterialError) {
  standard_material(
    color: color,
    metalness: 1.0,
    roughness: 0.8,
    map: option.None,
    normal_map: option.None,
    ambient_oclusion_map: option.None,
    roughness_map: option.None,
    metalness_map: option.None,
  )
}

/// Create a glossy material (non-metallic, very low roughness).
///
/// ## Example
///
/// ```gleam
/// let glossy_paint = scene.glossy(0xff0000)
/// ```
pub fn glossy(color color: Int) -> Result(Material, MaterialError) {
  standard_material(
    color: color,
    metalness: 0.0,
    roughness: 0.1,
    map: option.None,
    normal_map: option.None,
    ambient_oclusion_map: option.None,
    roughness_map: option.None,
    metalness_map: option.None,
  )
}

/// Create a matte material (non-metallic, very high roughness).
///
/// ## Example
///
/// ```gleam
/// let fabric = scene.matte(0x4a4a4a)
/// ```
pub fn matte(color color: Int) -> Result(Material, MaterialError) {
  standard_material(
    color: color,
    metalness: 0.0,
    roughness: 1.0,
    map: option.None,
    normal_map: option.None,
    ambient_oclusion_map: option.None,
    roughness_map: option.None,
    metalness_map: option.None,
  )
}

// --- Material Builder Pattern ---

/// Builder for standard (PBR) materials with sensible defaults.
///
/// Start with `new_standard_material()`, chain setter methods, then call `build()`.
///
/// ## Example
///
/// ```gleam
/// let material = scene.new_standard_material()
///   |> scene.mat_color(0xff0000)
///   |> scene.mat_metalness(0.8)
///   |> scene.mat_roughness(0.3)
///   |> scene.build_material()
/// ```
pub opaque type StandardMaterialBuilder {
  StandardMaterialBuilder(
    color: Int,
    metalness: Float,
    roughness: Float,
    map: Option(Texture),
    normal_map: Option(Texture),
    ambient_oclusion_map: Option(Texture),
    roughness_map: Option(Texture),
    metalness_map: Option(Texture),
  )
}

/// Create a new standard material builder with default values.
///
/// Defaults: gray color (0x808080), metalness 0.5, roughness 0.5, no textures.
pub fn new_standard_material() -> StandardMaterialBuilder {
  StandardMaterialBuilder(
    color: 0x808080,
    metalness: 0.5,
    roughness: 0.5,
    map: option.None,
    normal_map: option.None,
    ambient_oclusion_map: option.None,
    roughness_map: option.None,
    metalness_map: option.None,
  )
}

/// Set the base color of the material.
pub fn mat_color(
  builder: StandardMaterialBuilder,
  color: Int,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, color: color)
}

/// Set the metalness (0.0 = dielectric, 1.0 = metal).
pub fn mat_metalness(
  builder: StandardMaterialBuilder,
  metalness: Float,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, metalness: metalness)
}

/// Set the roughness (0.0 = mirror smooth, 1.0 = completely rough).
pub fn mat_roughness(
  builder: StandardMaterialBuilder,
  roughness: Float,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, roughness: roughness)
}

/// Set the color/diffuse texture map.
pub fn mat_map(
  builder: StandardMaterialBuilder,
  map: Texture,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, map: option.Some(map))
}

/// Set the normal map for surface detail.
pub fn mat_normal_map(
  builder: StandardMaterialBuilder,
  normal_map: Texture,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, normal_map: option.Some(normal_map))
}

/// Set the ambient occlusion map.
pub fn mat_ao_map(
  builder: StandardMaterialBuilder,
  ao_map: Texture,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, ambient_oclusion_map: option.Some(ao_map))
}

/// Set the roughness map.
pub fn mat_roughness_map(
  builder: StandardMaterialBuilder,
  roughness_map: Texture,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, roughness_map: option.Some(roughness_map))
}

/// Set the metalness map.
pub fn mat_metalness_map(
  builder: StandardMaterialBuilder,
  metalness_map: Texture,
) -> StandardMaterialBuilder {
  StandardMaterialBuilder(..builder, metalness_map: option.Some(metalness_map))
}

/// Build the material from the builder (validates parameters).
pub fn build_material(
  builder: StandardMaterialBuilder,
) -> Result(Material, MaterialError) {
  standard_material(
    color: builder.color,
    metalness: builder.metalness,
    roughness: builder.roughness,
    map: builder.map,
    normal_map: builder.normal_map,
    ambient_oclusion_map: builder.ambient_oclusion_map,
    roughness_map: builder.roughness_map,
    metalness_map: builder.metalness_map,
  )
}

// --- Geometry Builder Pattern ---

/// Builder for box geometry with sensible defaults.
pub opaque type BoxBuilder {
  BoxBuilder(width: Float, height: Float, depth: Float)
}

/// Create a new box builder (default: 1x1x1 cube).
pub fn new_box() -> BoxBuilder {
  BoxBuilder(width: 1.0, height: 1.0, depth: 1.0)
}

/// Set the width of the box.
pub fn box_width(builder: BoxBuilder, width: Float) -> BoxBuilder {
  BoxBuilder(..builder, width: width)
}

/// Set the height of the box.
pub fn box_height(builder: BoxBuilder, height: Float) -> BoxBuilder {
  BoxBuilder(..builder, height: height)
}

/// Set the depth of the box.
pub fn box_depth(builder: BoxBuilder, depth: Float) -> BoxBuilder {
  BoxBuilder(..builder, depth: depth)
}

/// Set all dimensions at once.
pub fn box_size(
  _builder: BoxBuilder,
  width width: Float,
  height height: Float,
  depth depth: Float,
) -> BoxBuilder {
  BoxBuilder(width: width, height: height, depth: depth)
}

/// Set uniform size (width = height = depth) for a cube.
pub fn box_cube(_builder: BoxBuilder, size: Float) -> BoxBuilder {
  BoxBuilder(width: size, height: size, depth: size)
}

/// Build the box geometry (validates parameters).
pub fn build_box(builder: BoxBuilder) -> Result(Geometry, GeometryError) {
  box(width: builder.width, height: builder.height, depth: builder.depth)
}

/// Builder for sphere geometry with sensible defaults.
pub opaque type SphereBuilder {
  SphereBuilder(radius: Float, width_segments: Int, height_segments: Int)
}

/// Create a new sphere builder (default: radius 1.0, 32x16 segments).
pub fn new_sphere() -> SphereBuilder {
  SphereBuilder(radius: 1.0, width_segments: 32, height_segments: 16)
}

/// Set the radius of the sphere.
pub fn sphere_radius(builder: SphereBuilder, radius: Float) -> SphereBuilder {
  SphereBuilder(..builder, radius: radius)
}

/// Set the width segments (horizontal divisions).
pub fn sphere_width_segments(
  builder: SphereBuilder,
  segments: Int,
) -> SphereBuilder {
  SphereBuilder(..builder, width_segments: segments)
}

/// Set the height segments (vertical divisions).
pub fn sphere_height_segments(
  builder: SphereBuilder,
  segments: Int,
) -> SphereBuilder {
  SphereBuilder(..builder, height_segments: segments)
}

/// Set both width and height segments (for uniform detail).
pub fn sphere_segments(builder: SphereBuilder, segments: Int) -> SphereBuilder {
  SphereBuilder(
    ..builder,
    width_segments: segments,
    height_segments: segments / 2,
  )
}

/// Build the sphere geometry (validates parameters).
pub fn build_sphere(builder: SphereBuilder) -> Result(Geometry, GeometryError) {
  sphere(
    radius: builder.radius,
    width_segments: builder.width_segments,
    height_segments: builder.height_segments,
  )
}

/// Builder for plane geometry with sensible defaults.
pub opaque type PlaneBuilder {
  PlaneBuilder(width: Float, height: Float)
}

/// Create a new plane builder (default: 1x1).
pub fn new_plane() -> PlaneBuilder {
  PlaneBuilder(width: 1.0, height: 1.0)
}

/// Set the width of the plane.
pub fn plane_width(builder: PlaneBuilder, width: Float) -> PlaneBuilder {
  PlaneBuilder(..builder, width: width)
}

/// Set the height of the plane.
pub fn plane_height(builder: PlaneBuilder, height: Float) -> PlaneBuilder {
  PlaneBuilder(..builder, height: height)
}

/// Set both dimensions at once.
pub fn plane_size(
  _builder: PlaneBuilder,
  width width: Float,
  height height: Float,
) -> PlaneBuilder {
  PlaneBuilder(width: width, height: height)
}

/// Set uniform size (square plane).
pub fn plane_square(_builder: PlaneBuilder, size: Float) -> PlaneBuilder {
  PlaneBuilder(width: size, height: size)
}

/// Build the plane geometry (validates parameters).
pub fn build_plane(builder: PlaneBuilder) -> Result(Geometry, GeometryError) {
  plane(width: builder.width, height: builder.height)
}

@internal
pub type Patch {
  AddNode(id: String, node: Node, parent_id: option.Option(String))
  RemoveNode(id: String)
  UpdateTransform(id: String, transform: transform.Transform)
  UpdateMaterial(id: String, material: Material)
  UpdateGeometry(id: String, geometry: Geometry)
  UpdateLight(id: String, light: Light)
  UpdateAnimation(id: String, animation: option.Option(AnimationPlayback))
  UpdatePhysics(id: String, physics: option.Option(RigidBody))
  UpdateAudio(id: String, config: AudioConfig)
  UpdateInstances(id: String, instances: List(transform.Transform))
  UpdateLODLevels(id: String, levels: List(LODLevel))
  UpdateCamera(
    id: String,
    camera_type: camera.Camera,
    look_at: option.Option(vec3.Vec3(Float)),
  )
  SetActiveCamera(id: String)
}

type NodeWithParent {
  NodeWithParent(node: Node, parent_id: option.Option(String))
}

fn flatten_scene(nodes: List(Node)) -> dict.Dict(String, NodeWithParent) {
  flatten_scene_helper(nodes, option.None, dict.new())
}

fn flatten_scene_helper(
  nodes: List(Node),
  parent_id: option.Option(String),
  acc: dict.Dict(String, NodeWithParent),
) -> dict.Dict(String, NodeWithParent) {
  list.fold(nodes, acc, fn(acc, node) {
    let acc = dict.insert(acc, node.id, NodeWithParent(node, parent_id))
    case node {
      Group(_, _, children) ->
        flatten_scene_helper(children, option.Some(node.id), acc)
      _ -> acc
    }
  })
}

@internal
pub fn diff(previous: List(Node), current: List(Node)) -> List(Patch) {
  let prev_dict = flatten_scene(previous)
  let curr_dict = flatten_scene(current)

  // Early exit: if both scenes are empty, no patches needed
  let prev_size = dict.size(prev_dict)
  let curr_size = dict.size(curr_dict)
  case prev_size == 0 && curr_size == 0 {
    True -> []
    False -> {
      // Convert to sets for O(log n) lookups instead of O(n) list.contains
      let prev_ids = dict.keys(prev_dict)
      let curr_ids = dict.keys(curr_dict)
      let prev_id_set = set.from_list(prev_ids)
      let curr_id_set = set.from_list(curr_ids)

      // Find removals: IDs in previous but not in current
      let removals =
        list.filter(prev_ids, fn(id) { !set.contains(curr_id_set, id) })
        |> list.map(fn(id) { RemoveNode(id) })

      // Find nodes that exist in both but have changed parents (need remove + add)
      let #(parent_changed_ids, same_parent_ids) =
        list.filter(curr_ids, fn(id) { set.contains(prev_id_set, id) })
        |> list.partition(fn(id) {
          case dict.get(prev_dict, id), dict.get(curr_dict, id) {
            Ok(NodeWithParent(_, prev_parent)),
              Ok(NodeWithParent(_, curr_parent))
            -> prev_parent != curr_parent
            _, _ -> False
          }
        })

      // For nodes with changed parents, treat as remove + add
      let parent_change_removals =
        list.map(parent_changed_ids, fn(id) { RemoveNode(id) })

      let parent_change_additions =
        list.filter_map(parent_changed_ids, fn(id) {
          case dict.get(curr_dict, id) {
            Ok(NodeWithParent(node, parent_id)) ->
              Ok(AddNode(id, node, parent_id))
            Error(_) -> Error(Nil)
          }
        })

      // Find additions: IDs in current but not in previous
      // Sort additions so parents are added before children
      let additions =
        list.filter(curr_ids, fn(id) { !set.contains(prev_id_set, id) })
        |> list.filter_map(fn(id) {
          case dict.get(curr_dict, id) {
            Ok(NodeWithParent(node, parent_id)) ->
              Ok(AddNode(id, node, parent_id))
            Error(_) -> Error(Nil)
          }
        })
        |> list.append(parent_change_additions)
        |> sort_patches_by_hierarchy(curr_dict)

      // Find updates: IDs in both with same parent, compare node properties
      let updates =
        list.flat_map(same_parent_ids, fn(id) {
          case dict.get(prev_dict, id), dict.get(curr_dict, id) {
            Ok(NodeWithParent(prev_node, _)), Ok(NodeWithParent(curr_node, _)) ->
              compare_nodes(id, prev_node, curr_node)
            _, _ -> []
          }
        })

      // Batch patches by type for optimal renderer processing:
      // 1. Removals first (free resources)
      // 2. Updates (modify existing)
      // 3. Additions last (create new, already sorted by hierarchy)
      batch_patches(removals, parent_change_removals, updates, additions)
    }
  }
}

/// Batch patches by type for optimal rendering order
/// Optimized: Single-pass partitioning + manual concatenation (no list.flatten)
fn batch_patches(
  removals: List(Patch),
  parent_change_removals: List(Patch),
  updates: List(Patch),
  additions: List(Patch),
) -> List(Patch) {
  // Single-pass partitioning with fold (O(n) instead of O(3n))
  let #(transform_updates, material_updates, geometry_updates, misc_updates) =
    list.fold(updates, #([], [], [], []), fn(acc, patch) {
      let #(transforms, materials, geometries, misc) = acc
      case patch {
        UpdateTransform(_, _) -> #(
          [patch, ..transforms],
          materials,
          geometries,
          misc,
        )
        UpdateMaterial(_, _) -> #(
          transforms,
          [patch, ..materials],
          geometries,
          misc,
        )
        UpdateGeometry(_, _) -> #(
          transforms,
          materials,
          [patch, ..geometries],
          misc,
        )
        _ -> #(transforms, materials, geometries, [patch, ..misc])
      }
    })

  // Efficient concatenation: prepend in reverse order, then single reverse
  // This avoids list.flatten's multiple traversals
  concat_patches([
    removals,
    parent_change_removals,
    list.reverse(transform_updates),
    list.reverse(material_updates),
    list.reverse(geometry_updates),
    list.reverse(misc_updates),
    additions,
  ])
}

/// Efficiently concatenate multiple lists using fold + prepend
/// O(n) total instead of list.flatten's O(n * m)
fn concat_patches(lists: List(List(Patch))) -> List(Patch) {
  list.fold(lists, [], fn(acc, patches) {
    list.fold(patches, acc, fn(acc2, patch) { [patch, ..acc2] })
  })
  |> list.reverse
}

/// Sort AddNode patches so that parents are added before their children
/// Optimized: pre-compute depths as tuples to avoid dict lookups in comparator
fn sort_patches_by_hierarchy(
  patches: List(Patch),
  node_dict: dict.Dict(String, NodeWithParent),
) -> List(Patch) {
  // Pre-compute (depth, patch) tuples for efficient sorting
  let patches_with_depth =
    list.map(patches, fn(patch) {
      case patch {
        AddNode(_, _, parent_id) -> {
          let depth = calculate_depth(parent_id, node_dict, 0)
          #(depth, patch)
        }
        _ -> #(0, patch)
      }
    })

  // Sort tuples by depth (O(n log n) without dict lookups)
  list.sort(patches_with_depth, fn(a, b) {
    let #(depth_a, _) = a
    let #(depth_b, _) = b
    case depth_a < depth_b {
      True -> order.Lt
      False ->
        case depth_a > depth_b {
          True -> order.Gt
          False -> order.Eq
        }
    }
  })
  // Extract patches from tuples
  |> list.map(fn(tuple) {
    let #(_, patch) = tuple
    patch
  })
}

/// Calculate the depth of a node in the hierarchy (0 = root)
fn calculate_depth(
  parent_id: option.Option(String),
  node_dict: dict.Dict(String, NodeWithParent),
  current_depth: Int,
) -> Int {
  case parent_id {
    option.None -> current_depth
    option.Some(id) ->
      case dict.get(node_dict, id) {
        Ok(NodeWithParent(_, parent_parent_id)) ->
          calculate_depth(parent_parent_id, node_dict, current_depth + 1)
        Error(_) -> current_depth + 1
      }
  }
}

/// Compare two nodes and generate update patches
fn compare_nodes(id: String, prev: Node, curr: Node) -> List(Patch) {
  // Fast path: if nodes are structurally equal, skip all field comparisons
  case prev == curr {
    True -> []
    False -> compare_nodes_detailed(id, prev, curr)
  }
}

/// Detailed comparison of node properties (called only when nodes differ)
/// Uses accumulator pattern to avoid empty list allocations
fn compare_nodes_detailed(id: String, prev: Node, curr: Node) -> List(Patch) {
  case prev, curr {
    Mesh(_, prev_geom, prev_mat, prev_trans, prev_phys),
      Mesh(_, curr_geom, curr_mat, curr_trans, curr_phys)
    ->
      compare_mesh_fields(
        id,
        prev_geom,
        prev_mat,
        prev_trans,
        prev_phys,
        curr_geom,
        curr_mat,
        curr_trans,
        curr_phys,
      )

    InstancedMesh(_, prev_geom, prev_mat, prev_instances),
      InstancedMesh(_, curr_geom, curr_mat, curr_instances)
    ->
      compare_instanced_mesh_fields(
        id,
        prev_geom,
        prev_mat,
        prev_instances,
        curr_geom,
        curr_mat,
        curr_instances,
      )

    Light(_, prev_light, prev_trans), Light(_, curr_light, curr_trans) ->
      compare_light_fields(id, prev_light, prev_trans, curr_light, curr_trans)

    Group(_, prev_trans, _), Group(_, curr_trans, _) ->
      case prev_trans != curr_trans {
        True -> [UpdateTransform(id, curr_trans)]
        False -> []
      }

    Camera(_, prev_cam, prev_trans, prev_look_at, prev_active, prev_viewport),
      Camera(_, curr_cam, curr_trans, curr_look_at, curr_active, curr_viewport)
    ->
      compare_camera_fields(
        id,
        prev_cam,
        prev_trans,
        prev_look_at,
        prev_active,
        prev_viewport,
        curr_cam,
        curr_trans,
        curr_look_at,
        curr_active,
        curr_viewport,
      )

    LOD(_, prev_levels, prev_trans), LOD(_, curr_levels, curr_trans) ->
      compare_lod_fields(id, prev_levels, prev_trans, curr_levels, curr_trans)

    Model3D(_, _, prev_trans, prev_anim, prev_phys),
      Model3D(_, _, curr_trans, curr_anim, curr_phys)
    ->
      compare_model3d_fields(
        id,
        prev_trans,
        prev_anim,
        prev_phys,
        curr_trans,
        curr_anim,
        curr_phys,
      )

    Audio(_, _, prev_config, _), Audio(_, _, curr_config, _) ->
      case prev_config != curr_config {
        True -> [UpdateAudio(id, curr_config)]
        False -> []
      }

    _, _ -> []
  }
}

/// Compare Mesh fields using accumulator pattern (no empty list allocations)
fn compare_mesh_fields(
  id: String,
  prev_geom: Geometry,
  prev_mat: Material,
  prev_trans: transform.Transform,
  prev_phys: option.Option(RigidBody),
  curr_geom: Geometry,
  curr_mat: Material,
  curr_trans: transform.Transform,
  curr_phys: option.Option(RigidBody),
) -> List(Patch) {
  let patches = []

  let patches = case prev_trans != curr_trans {
    True -> [UpdateTransform(id, curr_trans), ..patches]
    False -> patches
  }

  let patches = case prev_mat != curr_mat {
    True -> [UpdateMaterial(id, curr_mat), ..patches]
    False -> patches
  }

  let patches = case prev_geom != curr_geom {
    True -> [UpdateGeometry(id, curr_geom), ..patches]
    False -> patches
  }

  let patches = case prev_phys != curr_phys {
    True -> [UpdatePhysics(id, curr_phys), ..patches]
    False -> patches
  }

  patches
}

/// Compare InstancedMesh fields using accumulator pattern
fn compare_instanced_mesh_fields(
  id: String,
  prev_geom: Geometry,
  prev_mat: Material,
  prev_instances: List(transform.Transform),
  curr_geom: Geometry,
  curr_mat: Material,
  curr_instances: List(transform.Transform),
) -> List(Patch) {
  let patches = []

  let patches = case prev_mat != curr_mat {
    True -> [UpdateMaterial(id, curr_mat), ..patches]
    False -> patches
  }

  let patches = case prev_geom != curr_geom {
    True -> [UpdateGeometry(id, curr_geom), ..patches]
    False -> patches
  }

  let patches = case prev_instances != curr_instances {
    True -> [UpdateInstances(id, curr_instances), ..patches]
    False -> patches
  }

  patches
}

/// Compare Light fields using accumulator pattern
fn compare_light_fields(
  id: String,
  prev_light: Light,
  prev_trans: transform.Transform,
  curr_light: Light,
  curr_trans: transform.Transform,
) -> List(Patch) {
  let patches = []

  let patches = case prev_trans != curr_trans {
    True -> [UpdateTransform(id, curr_trans), ..patches]
    False -> patches
  }

  let patches = case prev_light != curr_light {
    True -> [UpdateLight(id, curr_light), ..patches]
    False -> patches
  }

  patches
}

/// Compare LOD fields using accumulator pattern
fn compare_lod_fields(
  id: String,
  prev_levels: List(LODLevel),
  prev_trans: transform.Transform,
  curr_levels: List(LODLevel),
  curr_trans: transform.Transform,
) -> List(Patch) {
  let patches = []

  let patches = case prev_trans != curr_trans {
    True -> [UpdateTransform(id, curr_trans), ..patches]
    False -> patches
  }

  let patches = case prev_levels != curr_levels {
    True -> [UpdateLODLevels(id, curr_levels), ..patches]
    False -> patches
  }

  patches
}

/// Compare Camera fields using accumulator pattern
fn compare_camera_fields(
  id: String,
  prev_cam: camera.Camera,
  prev_trans: transform.Transform,
  prev_look_at: option.Option(vec3.Vec3(Float)),
  prev_active: Bool,
  prev_viewport: option.Option(#(Int, Int, Int, Int)),
  curr_cam: camera.Camera,
  curr_trans: transform.Transform,
  curr_look_at: option.Option(vec3.Vec3(Float)),
  curr_active: Bool,
  curr_viewport: option.Option(#(Int, Int, Int, Int)),
) -> List(Patch) {
  let patches = []

  let patches = case prev_trans != curr_trans {
    True -> [UpdateTransform(id, curr_trans), ..patches]
    False -> patches
  }

  // If camera config, look_at, or viewport changed, emit UpdateCamera patch
  let patches = case
    prev_cam != curr_cam
    || prev_look_at != curr_look_at
    || prev_viewport != curr_viewport
  {
    True -> [UpdateCamera(id, curr_cam, curr_look_at), ..patches]
    False -> patches
  }

  // If active state changed, emit SetActiveCamera patch
  let patches = case prev_active, curr_active {
    False, True -> [SetActiveCamera(id), ..patches]
    _, _ -> patches
  }

  patches
}

/// Compare Model3D fields using accumulator pattern
fn compare_model3d_fields(
  id: String,
  prev_trans: transform.Transform,
  prev_anim: option.Option(AnimationPlayback),
  prev_phys: option.Option(RigidBody),
  curr_trans: transform.Transform,
  curr_anim: option.Option(AnimationPlayback),
  curr_phys: option.Option(RigidBody),
) -> List(Patch) {
  let patches = []

  let patches = case prev_trans != curr_trans {
    True -> [UpdateTransform(id, curr_trans), ..patches]
    False -> patches
  }

  let patches = case prev_anim != curr_anim {
    True -> [UpdateAnimation(id, curr_anim), ..patches]
    False -> patches
  }

  let patches = case prev_phys != curr_phys {
    True -> [UpdatePhysics(id, curr_phys), ..patches]
    False -> patches
  }

  patches
}

pub fn create_geometry(geometry: Geometry) -> Nil {
  case geometry {
    BoxGeometry(width:, height:, depth:) ->
      create_box_geometry(width, height, depth)
    CircleGeometry(radius:, segments:) ->
      create_circle_geometry(radius, segments)
    ConeGeometry(radius:, height:, segments:) ->
      create_cone_geometry(radius, height, segments)
    CustomGeometry(buffer) -> create_custom_geometry(buffer)
    CylinderGeometry(radius_top:, radius_bottom:, height:, radial_segments:) ->
      create_cylinder_geometry(
        radius_top,
        radius_bottom,
        height,
        radial_segments,
      )
    IcosahedronGeometry(radius:, detail:) ->
      create_icosahedron_geometry(radius, detail)
    PlaneGeometry(width:, height:) -> create_plane_geometry(width, height)
    SphereGeometry(radius:, width_segments:, height_segments:) ->
      create_sphere_geometry(radius, width_segments, height_segments)
    TetrahedronGeometry(radius:, detail:) ->
      create_tetrahedron_geometry(radius, detail)
    TorusGeometry(radius:, tube:, radial_segments:, tubular_segments:) ->
      create_torus_geometry(radius, tube, radial_segments, tubular_segments)
  }
}

@external(javascript, "./ffi/renderer.mjs", "createCustomGeometry")
fn create_custom_geometry(buffer: BufferGeometry) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createConeGeometry")
fn create_cone_geometry(radius: Float, height: Float, segments: Int) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createCircleGeometry")
fn create_circle_geometry(radius: Float, segments: Int) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createBoxGeometry")
fn create_box_geometry(width: Float, height: Float, depth: Float) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createCylinderGeometry")
fn create_cylinder_geometry(
  radius_top: Float,
  radius_bottom: Float,
  height: Float,
  radial_segments: Int,
) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createIcosahedronGeometry")
fn create_icosahedron_geometry(radius: Float, detail: Int) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createPlaneGeometry")
fn create_plane_geometry(width: Float, height: Float) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createSphereGeometry")
fn create_sphere_geometry(
  radius: Float,
  width_segments: Int,
  height_segments: Int,
) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createTetrahedronGeometry")
fn create_tetrahedron_geometry(radius: Float, detail: Int) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createTorusGeometry")
fn create_torus_geometry(
  radius: Float,
  tube: Float,
  radial_segments: Int,
  tubular_segments: Int,
) -> Nil

pub fn create_material(material: Material) -> Nil {
  case material {
    BasicMaterial(color:, map:, normal_map:, transparent:, opacity:) ->
      create_basic_material(color, transparent, opacity, map, normal_map)
    StandardMaterial(
      color:,
      map:,
      normal_map:,
      ambient_oclusion_map:,
      roughness_map:,
      metalness_map:,
      metalness:,
      roughness:,
    ) ->
      create_standard_material(
        color,
        metalness,
        roughness,
        map,
        normal_map,
        ambient_oclusion_map,
        roughness_map,
        metalness_map,
      )
    PhongMaterial(color:, map:, normal_map:, ambient_oclusion_map:, shininess:) ->
      create_phong_material(
        color,
        shininess,
        map,
        normal_map,
        ambient_oclusion_map,
      )
    LambertMaterial(color:, map:, normal_map:, ambient_oclusion_map:) ->
      create_lambert_material(color, map, normal_map, ambient_oclusion_map)
    ToonMaterial(color:, map:, normal_map:, ambient_oclusion_map:) ->
      create_toon_material(color, map, normal_map, ambient_oclusion_map)
    LineMaterial(color:, linewidth:) -> create_line_material(color, linewidth)
    SpriteMaterial(color:, map:, normal_map:, transparent:, opacity:) ->
      create_sprite_material(color, transparent, opacity, map, normal_map)
  }
}

@external(javascript, "./ffi/renderer.mjs", "createBasicMaterial")
fn create_basic_material(
  color: Int,
  transparent: Bool,
  opacity: Float,
  map: Option(Texture),
  normal_map: Option(Texture),
) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createStandardMaterial")
fn create_standard_material(
  color: Int,
  metalness: Float,
  roughness: Float,
  map: Option(Texture),
  normal_map: Option(Texture),
  ao_map: Option(Texture),
  roughness_map: Option(Texture),
  metalness_map: Option(Texture),
) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createPhongMaterial")
fn create_phong_material(
  color: Int,
  shininess: Float,
  map: Option(Texture),
  normal_map: Option(Texture),
  ao_map: Option(Texture),
) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createLambertMaterial")
fn create_lambert_material(
  color: Int,
  map: Option(Texture),
  normal_map: Option(Texture),
  ao_map: Option(Texture),
) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createToonMaterial")
fn create_toon_material(
  color: Int,
  map: Option(Texture),
  normal_map: Option(Texture),
  ao_map: Option(Texture),
) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createLineMaterial")
fn create_line_material(color: Int, linewidth: Float) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createSpriteMaterial")
fn create_sprite_material(
  color: Int,
  transparent: Bool,
  opacity: Float,
  map: Option(Texture),
  normal_map: Option(Texture),
) -> Nil

pub fn create_light(light: Light) -> Nil {
  case light {
    AmbientLight(intensity:, color:) -> create_ambient_light(intensity, color)
    DirectionalLight(intensity:, color:) ->
      create_directional_light(intensity, color)
    PointLight(intensity:, color:, distance:) ->
      create_point_light(intensity, color, distance)
    SpotLight(intensity:, color:, distance:, angle:, penumbra:) ->
      create_spot_light(intensity, color, distance, angle, penumbra)
    HemisphereLight(intensity:, sky_color:, ground_color:) ->
      create_hemisphere_light(intensity, sky_color, ground_color)
  }
}

@external(javascript, "./ffi/renderer.mjs", "createAmbientLight")
fn create_ambient_light(intensity: Float, color: Int) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createDirectionalLight")
fn create_directional_light(intensity: Float, color: Int) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createPointLight")
fn create_point_light(intensity: Float, color: Int, distance: Float) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createSpotLight")
fn create_spot_light(
  intensity: Float,
  color: Int,
  distance: Float,
  angle: Float,
  penumbra: Float,
) -> Nil

@external(javascript, "./ffi/renderer.mjs", "createHemisphereLight")
fn create_hemisphere_light(
  intensity: Float,
  sky_color: Int,
  ground_color: Int,
) -> Nil
