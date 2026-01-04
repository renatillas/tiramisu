// Post-processing effects using Three.js EffectComposer
import * as THREE from 'three';
import { EffectComposer } from 'three/addons/postprocessing/EffectComposer.js';
import { RenderPass } from 'three/addons/postprocessing/RenderPass.js';
import { UnrealBloomPass } from 'three/addons/postprocessing/UnrealBloomPass.js';
import { ShaderPass } from 'three/addons/postprocessing/ShaderPass.js';
import { FXAAShader } from 'three/addons/shaders/FXAAShader.js';
import { FilmPass } from 'three/addons/postprocessing/FilmPass.js';
import { GlitchPass } from 'three/addons/postprocessing/GlitchPass.js';
import { ClearPass } from 'three/addons/postprocessing/ClearPass.js';
import { OutputPass } from 'three/addons/postprocessing/OutputPass.js';
import { Option$isSome, Option$Some$0 } from '../gleam_stdlib/gleam/option.mjs';
import * as CAMERA from './tiramisu/camera.mjs';
import {
  Pass$isRenderPass,
  Pass$isClearPass,
  Pass$isOutputPass,
  Pass$isBloomPass,
  Pass$isPixelatePass,
  Pass$isFilmPass,
  Pass$isVignettePass,
  Pass$isFXAAPass,
  Pass$isGlitchPass,
  Pass$isColorCorrectionPass,
  Pass$isCustomShaderPass,
  UniformValue$isFloatUniform,
  UniformValue$FloatUniform$0,
  UniformValue$isIntUniform,
  UniformValue$IntUniform$0,
  UniformValue$isVec2Uniform,
  UniformValue$Vec2Uniform$0,
  UniformValue$Vec2Uniform$1,
  UniformValue$isVec3Uniform,
  UniformValue$Vec3Uniform$0,
  UniformValue$Vec3Uniform$1,
  UniformValue$Vec3Uniform$2,
  UniformValue$isColorUniform,
  UniformValue$ColorUniform$0
} from './tiramisu/camera.mjs';

/**
 * Create an EffectComposer from a Gleam PostProcessing configuration
 * @param {THREE.WebGLRenderer} renderer - The WebGL renderer
 * @param {THREE.Scene} scene - The Three.js scene
 * @param {THREE.Camera} camera - The camera to render from
 * @param {Object} postProcessingConfig - Gleam PostProcessing object
 * @returns {EffectComposer} Configured EffectComposer
 */
export function createEffectComposer(renderer, scene, camera, postProcessingConfig) {
  const composer = new EffectComposer(renderer);

  // Add all user-defined passes in order
  const passes = CAMERA.get_passes(postProcessingConfig);
  const passesArray = passes.toArray();

  for (const gleamPass of passesArray) {
    const threePass = createPassFromGleam(gleamPass, renderer, scene, camera);
    if (threePass) {
      composer.addPass(threePass);
    }
  }

  return composer;
}

/**
 * Create a Three.js pass from a Gleam Pass type
 * @param {Object} gleamPass - Gleam Pass object
 * @param {THREE.WebGLRenderer} renderer - The WebGL renderer (for size info)
 * @param {THREE.Scene} scene - The Three.js scene
 * @param {THREE.Camera} camera - The camera
 * @returns {Object|null} Three.js pass or null if unsupported
 */
function createPassFromGleam(gleamPass, renderer, scene, camera) {
  // Pipeline passes (RenderPass, ClearPass, OutputPass)
  if (Pass$isRenderPass(gleamPass)) {
    return new RenderPass(scene, camera);
  }

  if (Pass$isClearPass(gleamPass)) {
    const colorOption = gleamPass.color;
    let clearColor;

    if (Option$isSome(colorOption)) {
      clearColor = new THREE.Color(Option$Some$0(colorOption));
    } else {
      // Use scene background color or fallback to black
      if (scene.background && scene.background.isColor) {
        clearColor = scene.background;
      } else {
        clearColor = new THREE.Color(0x000000);
      }
    }

    return new ClearPass(clearColor, 1.0);
  }

  if (Pass$isOutputPass(gleamPass)) {
    return new OutputPass();
  }

  // Effect passes
  if (Pass$isBloomPass(gleamPass)) {
    const { strength, threshold, radius } = gleamPass;
    const size = renderer.getSize(new THREE.Vector2());
    return new UnrealBloomPass(
      new THREE.Vector2(size.x, size.y),
      strength,
      radius,
      threshold
    );
  }

  if (Pass$isPixelatePass(gleamPass)) {
    const { pixel_size: pixelSize, normal_edge_strength: normalEdgeStrength, depth_edge_strength: depthEdgeStrength } = gleamPass;
    const size = renderer.getSize(new THREE.Vector2());

    const pixelateShader = {
      uniforms: {
        'tDiffuse': { value: null },
        'resolution': { value: new THREE.Vector2(size.x, size.y) },
        'pixelSize': { value: pixelSize },
        'normalEdgeStrength': { value: normalEdgeStrength },
        'depthEdgeStrength': { value: depthEdgeStrength }
      },
      vertexShader: `
        varying vec2 vUv;
        void main() {
          vUv = uv;
          gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
        }
      `,
      fragmentShader: `
        uniform sampler2D tDiffuse;
        uniform vec2 resolution;
        uniform float pixelSize;
        varying vec2 vUv;

        void main() {
          vec2 dxy = pixelSize / resolution;
          vec2 coord = dxy * floor(vUv / dxy);
          gl_FragColor = texture2D(tDiffuse, coord);
        }
      `
    };

    return new ShaderPass(pixelateShader);
  }

  if (Pass$isFilmPass(gleamPass)) {
    const { noise_intensity: noiseIntensity, grayscale } = gleamPass;
    return new FilmPass(noiseIntensity, grayscale);
  }

  if (Pass$isVignettePass(gleamPass)) {
    const { darkness, offset } = gleamPass;

    const vignetteShader = {
      uniforms: {
        'tDiffuse': { value: null },
        'darkness': { value: darkness },
        'offset': { value: offset }
      },
      vertexShader: `
        varying vec2 vUv;
        void main() {
          vUv = uv;
          gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
        }
      `,
      fragmentShader: `
        uniform sampler2D tDiffuse;
        uniform float darkness;
        uniform float offset;
        varying vec2 vUv;

        void main() {
          vec4 texel = texture2D(tDiffuse, vUv);
          vec2 uv = (vUv - 0.5) * 2.0;
          float dist = length(uv);
          float vignette = smoothstep(offset, offset - darkness, dist);
          texel.rgb *= vignette;
          gl_FragColor = texel;
        }
      `
    };

    return new ShaderPass(vignetteShader);
  }

  if (Pass$isFXAAPass(gleamPass)) {
    const size = renderer.getSize(new THREE.Vector2());
    const fxaaPass = new ShaderPass(FXAAShader);
    fxaaPass.material.uniforms['resolution'].value.x = 1 / size.x;
    fxaaPass.material.uniforms['resolution'].value.y = 1 / size.y;
    return fxaaPass;
  }

  if (Pass$isGlitchPass(gleamPass)) {
    return new GlitchPass(gleamPass.dt_size);
  }

  if (Pass$isColorCorrectionPass(gleamPass)) {
    const { brightness, contrast, saturation } = gleamPass;

    const colorCorrectionShader = {
      uniforms: {
        'tDiffuse': { value: null },
        'brightness': { value: brightness },
        'contrast': { value: contrast },
        'saturation': { value: saturation }
      },
      vertexShader: `
        varying vec2 vUv;
        void main() {
          vUv = uv;
          gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
        }
      `,
      fragmentShader: `
        uniform sampler2D tDiffuse;
        uniform float brightness;
        uniform float contrast;
        uniform float saturation;
        varying vec2 vUv;

        void main() {
          vec4 texel = texture2D(tDiffuse, vUv);

          // Brightness
          texel.rgb += brightness;

          // Contrast
          texel.rgb = (texel.rgb - 0.5) * (1.0 + contrast) + 0.5;

          // Saturation
          float gray = dot(texel.rgb, vec3(0.299, 0.587, 0.114));
          texel.rgb = mix(vec3(gray), texel.rgb, 1.0 + saturation);

          gl_FragColor = texel;
        }
      `
    };

    return new ShaderPass(colorCorrectionShader);
  }

  if (Pass$isCustomShaderPass(gleamPass)) {
    const { vertex_shader: vertexShader, fragment_shader: fragmentShader, uniforms } = gleamPass;

    // Convert Gleam uniforms to Three.js format
    const threeUniforms = { 'tDiffuse': { value: null } };
    const uniformsArray = uniforms.toArray();

    for (const [name, value] of uniformsArray) {
      if (UniformValue$isFloatUniform(value)) {
        threeUniforms[name] = { value: UniformValue$FloatUniform$0(value) };
      } else if (UniformValue$isIntUniform(value)) {
        threeUniforms[name] = { value: UniformValue$IntUniform$0(value) };
      } else if (UniformValue$isVec2Uniform(value)) {
        threeUniforms[name] = { value: new THREE.Vector2(UniformValue$Vec2Uniform$0(value), UniformValue$Vec2Uniform$1(value)) };
      } else if (UniformValue$isVec3Uniform(value)) {
        threeUniforms[name] = { value: new THREE.Vector3(UniformValue$Vec3Uniform$0(value), UniformValue$Vec3Uniform$1(value), UniformValue$Vec3Uniform$2(value)) };
      } else if (UniformValue$isColorUniform(value)) {
        threeUniforms[name] = { value: new THREE.Color(UniformValue$ColorUniform$0(value)) };
      }
    }

    const customShader = {
      uniforms: threeUniforms,
      vertexShader: vertexShader,
      fragmentShader: fragmentShader
    };

    return new ShaderPass(customShader);
  }

  // Unknown pass type
  console.warn('[Postprocessing] Unknown pass type:', gleamPass);
  return null;
}
