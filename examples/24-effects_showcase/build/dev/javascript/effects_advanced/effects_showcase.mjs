import * as $float from "../gleam_stdlib/gleam/float.mjs";
import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../gleam_stdlib/gleam/option.mjs";
import * as $tiramisu from "../tiramisu/tiramisu.mjs";
import * as $background from "../tiramisu/tiramisu/background.mjs";
import * as $camera from "../tiramisu/tiramisu/camera.mjs";
import * as $effect from "../tiramisu/tiramisu/effect.mjs";
import * as $geometry from "../tiramisu/tiramisu/geometry.mjs";
import * as $input from "../tiramisu/tiramisu/input.mjs";
import * as $light from "../tiramisu/tiramisu/light.mjs";
import * as $material from "../tiramisu/tiramisu/material.mjs";
import * as $scene from "../tiramisu/tiramisu/scene.mjs";
import * as $transform from "../tiramisu/tiramisu/transform.mjs";
import * as $vec3 from "../vec/vec/vec3.mjs";
import { Ok, toList, CustomType as $CustomType, makeError } from "./gleam.mjs";

const FILEPATH = "src/effects_showcase.gleam";

export class Model extends $CustomType {
  constructor(rotation, cube_scale, cube_color, tween_value, tween_running, current_easing, interval_active, interval_count, status_message, is_fullscreen, is_pointer_locked, clipboard_text) {
    super();
    this.rotation = rotation;
    this.cube_scale = cube_scale;
    this.cube_color = cube_color;
    this.tween_value = tween_value;
    this.tween_running = tween_running;
    this.current_easing = current_easing;
    this.interval_active = interval_active;
    this.interval_count = interval_count;
    this.status_message = status_message;
    this.is_fullscreen = is_fullscreen;
    this.is_pointer_locked = is_pointer_locked;
    this.clipboard_text = clipboard_text;
  }
}

export class DelayedMessage extends $CustomType {}

export class TimeoutTriggered extends $CustomType {}

export class IntervalTick extends $CustomType {}

export class TweenUpdate extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class TweenComplete extends $CustomType {}

export class StartInterval extends $CustomType {}

export class StopInterval extends $CustomType {}

export class StartTween extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class ToggleFullscreen extends $CustomType {}

export class FullscreenSuccess extends $CustomType {}

export class FullscreenError extends $CustomType {}

export class TogglePointerLock extends $CustomType {}

export class PointerLockSuccess extends $CustomType {}

export class PointerLockError extends $CustomType {}

export class TriggerVibration extends $CustomType {}

export class CopyToClipboard extends $CustomType {}

export class ClipboardWriteSuccess extends $CustomType {}

export class ClipboardWriteError extends $CustomType {}

export class PasteFromClipboard extends $CustomType {}

export class ClipboardRead extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class ClipboardReadError extends $CustomType {}

export class KeyPress extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Tick extends $CustomType {}

function init(_) {
  let model = new Model(
    0.0,
    1.0,
    0x3498db,
    0.0,
    false,
    new $effect.Linear(),
    false,
    0,
    "Press keys to trigger effects! [Space] Start interval, [S] Stop interval, [1-7] Tween easings, [F] Fullscreen, [L] Pointer lock, [V] Vibrate, [C] Copy, [P] Paste",
    false,
    false,
    "Hello from Tiramisu!",
  );
  let welcome_effect = $effect.delay(2000, new DelayedMessage());
  return [model, welcome_effect, new $option.None()];
}

function view(model, _) {
  let $ = $camera.perspective(75.0, 0.1, 1000.0);
  let cam;
  if ($ instanceof Ok) {
    cam = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "effects_showcase",
      441,
      "view",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 11012,
        end: 11100,
        pattern_start: 11023,
        pattern_end: 11030
      }
    )
  }
  let $1 = $geometry.box(1.0, 1.0, 1.0);
  let cube_geometry;
  if ($1 instanceof Ok) {
    cube_geometry = $1[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "effects_showcase",
      445,
      "view",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $1,
        start: 11145,
        end: 11229,
        pattern_start: 11156,
        pattern_end: 11173
      }
    )
  }
  let _block;
  let _pipe = $material.new$();
  let _pipe$1 = $material.with_color(_pipe, model.cube_color);
  let _pipe$2 = $material.with_metalness(_pipe$1, 0.3);
  let _pipe$3 = $material.with_roughness(_pipe$2, 0.7);
  _block = $material.build(_pipe$3);
  let $2 = _block;
  let cube_material;
  if ($2 instanceof Ok) {
    cube_material = $2[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "effects_showcase",
      448,
      "view",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $2,
        start: 11233,
        end: 11423,
        pattern_start: 11244,
        pattern_end: 11261
      }
    )
  }
  let $3 = $geometry.plane(10.0, 10.0);
  let plane_geometry;
  if ($3 instanceof Ok) {
    plane_geometry = $3[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "effects_showcase",
      456,
      "view",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $3,
        start: 11449,
        end: 11522,
        pattern_start: 11460,
        pattern_end: 11478
      }
    )
  }
  let _block$1;
  let _pipe$4 = $material.new$();
  let _pipe$5 = $material.with_color(_pipe$4, 0x2c3e50);
  let _pipe$6 = $material.with_metalness(_pipe$5, 0.1);
  let _pipe$7 = $material.with_roughness(_pipe$6, 0.9);
  _block$1 = $material.build(_pipe$7);
  let $4 = _block$1;
  let plane_material;
  if ($4 instanceof Ok) {
    plane_material = $4[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "effects_showcase",
      458,
      "view",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $4,
        start: 11526,
        end: 11709,
        pattern_start: 11537,
        pattern_end: 11555
      }
    )
  }
  let $5 = $light.ambient(0.3, 0xffffff);
  let ambient;
  if ($5 instanceof Ok) {
    ambient = $5[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "effects_showcase",
      466,
      "view",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $5,
        start: 11725,
        end: 11796,
        pattern_start: 11736,
        pattern_end: 11747
      }
    )
  }
  let $6 = $light.directional(0.8, 0xffffff);
  let directional;
  if ($6 instanceof Ok) {
    directional = $6[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "effects_showcase",
      467,
      "view",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $6,
        start: 11799,
        end: 11882,
        pattern_start: 11810,
        pattern_end: 11825
      }
    )
  }
  return toList([
    new $scene.Camera(
      "main",
      cam,
      $transform.at(new $vec3.Vec3(0.0, 2.0, 5.0)),
      new Some(new $vec3.Vec3(0.0, 0.0, 0.0)),
      true,
      new None(),
    ),
    new $scene.Light("ambient", ambient, $transform.identity),
    new $scene.Light(
      "sun",
      directional,
      $transform.at(new $vec3.Vec3(5.0, 5.0, 5.0)),
    ),
    new $scene.Mesh(
      "cube",
      cube_geometry,
      cube_material,
      (() => {
        let _pipe$8 = $transform.identity;
        let _pipe$9 = $transform.with_rotation(
          _pipe$8,
          new $vec3.Vec3(model.rotation, model.rotation * 1.3, 0.0),
        );
        return $transform.scale_by(
          _pipe$9,
          new $vec3.Vec3(model.cube_scale, model.cube_scale, model.cube_scale),
        );
      })(),
      new $option.None(),
    ),
    new $scene.Mesh(
      "plane",
      plane_geometry,
      plane_material,
      $transform.at(new $vec3.Vec3(0.0, -2.0, -3.0)),
      new $option.None(),
    ),
  ]);
}

function easing_to_string(easing) {
  if (easing instanceof $effect.Linear) {
    return "Linear";
  } else if (easing instanceof $effect.EaseInQuad) {
    return "EaseInQuad";
  } else if (easing instanceof $effect.EaseOutQuad) {
    return "EaseOutQuad";
  } else if (easing instanceof $effect.EaseInOutQuad) {
    return "EaseInOutQuad";
  } else if (easing instanceof $effect.EaseInCubic) {
    return "EaseInCubic";
  } else if (easing instanceof $effect.EaseOutCubic) {
    return "EaseOutCubic";
  } else {
    return "EaseInOutCubic";
  }
}

function update(loop$model, loop$msg, loop$ctx) {
  while (true) {
    let model = loop$model;
    let msg = loop$msg;
    let ctx = loop$ctx;
    if (msg instanceof DelayedMessage) {
      return [
        new Model(
          model.rotation,
          model.cube_scale,
          model.cube_color,
          model.tween_value,
          model.tween_running,
          model.current_easing,
          model.interval_active,
          model.interval_count,
          "Delayed message received after 2 seconds!",
          model.is_fullscreen,
          model.is_pointer_locked,
          model.clipboard_text,
        ),
        $effect.none(),
        new $option.None(),
      ];
    } else if (msg instanceof TimeoutTriggered) {
      return [
        new Model(
          model.rotation,
          model.cube_scale,
          model.cube_color,
          model.tween_value,
          model.tween_running,
          model.current_easing,
          model.interval_active,
          model.interval_count,
          "Timeout triggered! Color changed.",
          model.is_fullscreen,
          model.is_pointer_locked,
          model.clipboard_text,
        ),
        $effect.none(),
        new $option.None(),
      ];
    } else if (msg instanceof IntervalTick) {
      let new_count = model.interval_count + 1;
      let _block;
      let $ = new_count % 3;
      if ($ === 0) {
        _block = 0x3498db;
      } else if ($ === 1) {
        _block = 0xe74c3c;
      } else {
        _block = 0x2ecc71;
      }
      let new_color = _block;
      return [
        new Model(
          model.rotation,
          model.cube_scale,
          new_color,
          model.tween_value,
          model.tween_running,
          model.current_easing,
          model.interval_active,
          new_count,
          "Interval tick #" + $int.to_string(new_count),
          model.is_fullscreen,
          model.is_pointer_locked,
          model.clipboard_text,
        ),
        $effect.none(),
        new $option.None(),
      ];
    } else if (msg instanceof TweenUpdate) {
      let value = msg[0];
      return [
        new Model(
          model.rotation,
          value,
          model.cube_color,
          value,
          model.tween_running,
          model.current_easing,
          model.interval_active,
          model.interval_count,
          model.status_message,
          model.is_fullscreen,
          model.is_pointer_locked,
          model.clipboard_text,
        ),
        $effect.none(),
        new $option.None(),
      ];
    } else if (msg instanceof TweenComplete) {
      let reverse_tween = $effect.tween(
        2.0,
        1.0,
        1000,
        model.current_easing,
        (var0) => { return new TweenUpdate(var0); },
        new TweenComplete(),
      );
      return [
        new Model(
          model.rotation,
          model.cube_scale,
          model.cube_color,
          model.tween_value,
          false,
          model.current_easing,
          model.interval_active,
          model.interval_count,
          "Tween complete!",
          model.is_fullscreen,
          model.is_pointer_locked,
          model.clipboard_text,
        ),
        reverse_tween,
        new $option.None(),
      ];
    } else if (msg instanceof StartInterval) {
      let $ = model.interval_active;
      if ($) {
        return [model, $effect.none(), new $option.None()];
      } else {
        let interval_effect = $effect.interval(
          "demo_interval",
          1000,
          new IntervalTick(),
        );
        return [
          new Model(
            model.rotation,
            model.cube_scale,
            model.cube_color,
            model.tween_value,
            model.tween_running,
            model.current_easing,
            true,
            0,
            "Interval started - cube will pulse every second",
            model.is_fullscreen,
            model.is_pointer_locked,
            model.clipboard_text,
          ),
          interval_effect,
          new $option.None(),
        ];
      }
    } else if (msg instanceof StopInterval) {
      let $ = model.interval_active;
      if ($) {
        let cancel_effect = $effect.cancel_interval("demo_interval");
        return [
          new Model(
            model.rotation,
            model.cube_scale,
            model.cube_color,
            model.tween_value,
            model.tween_running,
            model.current_easing,
            false,
            model.interval_count,
            "Interval stopped",
            model.is_fullscreen,
            model.is_pointer_locked,
            model.clipboard_text,
          ),
          cancel_effect,
          new $option.None(),
        ];
      } else {
        return [model, $effect.none(), new $option.None()];
      }
    } else if (msg instanceof StartTween) {
      let easing = msg[0];
      let $ = model.tween_running;
      if ($) {
        return [model, $effect.none(), new $option.None()];
      } else {
        let tween_effect = $effect.tween(
          0.5,
          2.0,
          2000,
          easing,
          (var0) => { return new TweenUpdate(var0); },
          new TweenComplete(),
        );
        let easing_name = easing_to_string(easing);
        return [
          new Model(
            model.rotation,
            model.cube_scale,
            model.cube_color,
            model.tween_value,
            true,
            easing,
            model.interval_active,
            model.interval_count,
            ("Tween started with " + easing_name) + " easing",
            model.is_fullscreen,
            model.is_pointer_locked,
            model.clipboard_text,
          ),
          tween_effect,
          new $option.None(),
        ];
      }
    } else if (msg instanceof ToggleFullscreen) {
      let $ = model.is_fullscreen;
      if ($) {
        return [
          new Model(
            model.rotation,
            model.cube_scale,
            model.cube_color,
            model.tween_value,
            model.tween_running,
            model.current_easing,
            model.interval_active,
            model.interval_count,
            "Exited fullscreen",
            false,
            model.is_pointer_locked,
            model.clipboard_text,
          ),
          $effect.exit_fullscreen(),
          new $option.None(),
        ];
      } else {
        let fullscreen_effect = $effect.request_fullscreen(
          new FullscreenSuccess(),
          new FullscreenError(),
        );
        return [model, fullscreen_effect, new $option.None()];
      }
    } else if (msg instanceof FullscreenSuccess) {
      return [
        new Model(
          model.rotation,
          model.cube_scale,
          model.cube_color,
          model.tween_value,
          model.tween_running,
          model.current_easing,
          model.interval_active,
          model.interval_count,
          "Entered fullscreen mode!",
          true,
          model.is_pointer_locked,
          model.clipboard_text,
        ),
        $effect.none(),
        new $option.None(),
      ];
    } else if (msg instanceof FullscreenError) {
      return [
        new Model(
          model.rotation,
          model.cube_scale,
          model.cube_color,
          model.tween_value,
          model.tween_running,
          model.current_easing,
          model.interval_active,
          model.interval_count,
          "Fullscreen request failed",
          model.is_fullscreen,
          model.is_pointer_locked,
          model.clipboard_text,
        ),
        $effect.none(),
        new $option.None(),
      ];
    } else if (msg instanceof TogglePointerLock) {
      let $ = model.is_pointer_locked;
      if ($) {
        return [
          new Model(
            model.rotation,
            model.cube_scale,
            model.cube_color,
            model.tween_value,
            model.tween_running,
            model.current_easing,
            model.interval_active,
            model.interval_count,
            "Exited pointer lock",
            model.is_fullscreen,
            false,
            model.clipboard_text,
          ),
          $effect.exit_pointer_lock(),
          new $option.None(),
        ];
      } else {
        let pointer_lock_effect = $effect.request_pointer_lock(
          new PointerLockSuccess(),
          new PointerLockError(),
        );
        return [model, pointer_lock_effect, new $option.None()];
      }
    } else if (msg instanceof PointerLockSuccess) {
      return [
        new Model(
          model.rotation,
          model.cube_scale,
          model.cube_color,
          model.tween_value,
          model.tween_running,
          model.current_easing,
          model.interval_active,
          model.interval_count,
          "Pointer locked! Press ESC to exit.",
          model.is_fullscreen,
          true,
          model.clipboard_text,
        ),
        $effect.none(),
        new $option.None(),
      ];
    } else if (msg instanceof PointerLockError) {
      return [
        new Model(
          model.rotation,
          model.cube_scale,
          model.cube_color,
          model.tween_value,
          model.tween_running,
          model.current_easing,
          model.interval_active,
          model.interval_count,
          "Pointer lock request failed",
          model.is_fullscreen,
          model.is_pointer_locked,
          model.clipboard_text,
        ),
        $effect.none(),
        new $option.None(),
      ];
    } else if (msg instanceof TriggerVibration) {
      let vibration_effect = $effect.vibrate(toList([200, 100, 200]));
      return [
        new Model(
          model.rotation,
          model.cube_scale,
          model.cube_color,
          model.tween_value,
          model.tween_running,
          model.current_easing,
          model.interval_active,
          model.interval_count,
          "Vibration triggered (mobile only)",
          model.is_fullscreen,
          model.is_pointer_locked,
          model.clipboard_text,
        ),
        vibration_effect,
        new $option.None(),
      ];
    } else if (msg instanceof CopyToClipboard) {
      let copy_effect = $effect.clipboard_write(
        model.clipboard_text,
        new ClipboardWriteSuccess(),
        new ClipboardWriteError(),
      );
      return [model, copy_effect, new $option.None()];
    } else if (msg instanceof ClipboardWriteSuccess) {
      return [
        new Model(
          model.rotation,
          model.cube_scale,
          model.cube_color,
          model.tween_value,
          model.tween_running,
          model.current_easing,
          model.interval_active,
          model.interval_count,
          ("Copied to clipboard: \"" + model.clipboard_text) + "\"",
          model.is_fullscreen,
          model.is_pointer_locked,
          model.clipboard_text,
        ),
        $effect.none(),
        new $option.None(),
      ];
    } else if (msg instanceof ClipboardWriteError) {
      return [
        new Model(
          model.rotation,
          model.cube_scale,
          model.cube_color,
          model.tween_value,
          model.tween_running,
          model.current_easing,
          model.interval_active,
          model.interval_count,
          "Failed to copy to clipboard",
          model.is_fullscreen,
          model.is_pointer_locked,
          model.clipboard_text,
        ),
        $effect.none(),
        new $option.None(),
      ];
    } else if (msg instanceof PasteFromClipboard) {
      let paste_effect = $effect.clipboard_read(
        (var0) => { return new ClipboardRead(var0); },
        new ClipboardReadError(),
      );
      return [model, paste_effect, new $option.None()];
    } else if (msg instanceof ClipboardRead) {
      let text = msg[0];
      return [
        new Model(
          model.rotation,
          model.cube_scale,
          model.cube_color,
          model.tween_value,
          model.tween_running,
          model.current_easing,
          model.interval_active,
          model.interval_count,
          ("Pasted from clipboard: \"" + text) + "\"",
          model.is_fullscreen,
          model.is_pointer_locked,
          text,
        ),
        $effect.none(),
        new $option.None(),
      ];
    } else if (msg instanceof ClipboardReadError) {
      return [
        new Model(
          model.rotation,
          model.cube_scale,
          model.cube_color,
          model.tween_value,
          model.tween_running,
          model.current_easing,
          model.interval_active,
          model.interval_count,
          "Failed to read from clipboard",
          model.is_fullscreen,
          model.is_pointer_locked,
          model.clipboard_text,
        ),
        $effect.none(),
        new $option.None(),
      ];
    } else if (msg instanceof KeyPress) {
      let key = msg[0];
      if (key instanceof $input.KeyC) {
        loop$model = model;
        loop$msg = new CopyToClipboard();
        loop$ctx = ctx;
      } else if (key instanceof $input.KeyF) {
        loop$model = model;
        loop$msg = new ToggleFullscreen();
        loop$ctx = ctx;
      } else if (key instanceof $input.KeyL) {
        loop$model = model;
        loop$msg = new TogglePointerLock();
        loop$ctx = ctx;
      } else if (key instanceof $input.KeyP) {
        loop$model = model;
        loop$msg = new PasteFromClipboard();
        loop$ctx = ctx;
      } else if (key instanceof $input.KeyS) {
        loop$model = model;
        loop$msg = new StopInterval();
        loop$ctx = ctx;
      } else if (key instanceof $input.KeyT) {
        let timeout_effect = $effect.timeout(3000, new TimeoutTriggered());
        let new_model = new Model(
          model.rotation,
          model.cube_scale,
          model.cube_color,
          model.tween_value,
          model.tween_running,
          model.current_easing,
          model.interval_active,
          model.interval_count,
          "Timeout set - color will change in 3 seconds",
          model.is_fullscreen,
          model.is_pointer_locked,
          model.clipboard_text,
        );
        return [new_model, timeout_effect, new $option.None()];
      } else if (key instanceof $input.KeyV) {
        loop$model = model;
        loop$msg = new TriggerVibration();
        loop$ctx = ctx;
      } else if (key instanceof $input.Digit1) {
        loop$model = model;
        loop$msg = new StartTween(new $effect.Linear());
        loop$ctx = ctx;
      } else if (key instanceof $input.Digit2) {
        loop$model = model;
        loop$msg = new StartTween(new $effect.EaseInQuad());
        loop$ctx = ctx;
      } else if (key instanceof $input.Digit3) {
        loop$model = model;
        loop$msg = new StartTween(new $effect.EaseOutQuad());
        loop$ctx = ctx;
      } else if (key instanceof $input.Digit4) {
        loop$model = model;
        loop$msg = new StartTween(new $effect.EaseInOutQuad());
        loop$ctx = ctx;
      } else if (key instanceof $input.Digit5) {
        loop$model = model;
        loop$msg = new StartTween(new $effect.EaseInCubic());
        loop$ctx = ctx;
      } else if (key instanceof $input.Digit6) {
        loop$model = model;
        loop$msg = new StartTween(new $effect.EaseOutCubic());
        loop$ctx = ctx;
      } else if (key instanceof $input.Digit7) {
        loop$model = model;
        loop$msg = new StartTween(new $effect.EaseInOutCubic());
        loop$ctx = ctx;
      } else if (key instanceof $input.Space) {
        loop$model = model;
        loop$msg = new StartInterval();
        loop$ctx = ctx;
      } else {
        return [model, $effect.none(), new $option.None()];
      }
    } else {
      let new_rotation = model.rotation + (ctx.delta_time * 0.5);
      return [
        new Model(
          new_rotation,
          model.cube_scale,
          model.cube_color,
          model.tween_value,
          model.tween_running,
          model.current_easing,
          model.interval_active,
          model.interval_count,
          model.status_message,
          model.is_fullscreen,
          model.is_pointer_locked,
          model.clipboard_text,
        ),
        $effect.none(),
        new $option.None(),
      ];
    }
  }
}

export function main() {
  return $tiramisu.run(
    new None(),
    new $background.Color(0x1a1a2e),
    init,
    update,
    view,
  );
}
