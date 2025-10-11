import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $io from "../gleam_stdlib/gleam/io.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
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
import {
  Ok,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
} from "./gleam.mjs";

const FILEPATH = "src/effects_showcase.gleam";

export class Model extends $CustomType {
  constructor(rotation, cube_scale, cube_color, tween_value, tween_running, current_easing, interval_id, interval_count, status_message, is_fullscreen, is_pointer_locked, clipboard_text, last_keys) {
    super();
    this.rotation = rotation;
    this.cube_scale = cube_scale;
    this.cube_color = cube_color;
    this.tween_value = tween_value;
    this.tween_running = tween_running;
    this.current_easing = current_easing;
    this.interval_id = interval_id;
    this.interval_count = interval_count;
    this.status_message = status_message;
    this.is_fullscreen = is_fullscreen;
    this.is_pointer_locked = is_pointer_locked;
    this.clipboard_text = clipboard_text;
    this.last_keys = last_keys;
  }
}

export class KeyPressState extends $CustomType {
  constructor(space, s, digit1, digit2, digit3, digit4, digit5, digit6, digit7, t, f, l, c, p, touch_screen_pressed) {
    super();
    this.space = space;
    this.s = s;
    this.digit1 = digit1;
    this.digit2 = digit2;
    this.digit3 = digit3;
    this.digit4 = digit4;
    this.digit5 = digit5;
    this.digit6 = digit6;
    this.digit7 = digit7;
    this.t = t;
    this.f = f;
    this.l = l;
    this.c = c;
    this.p = p;
    this.touch_screen_pressed = touch_screen_pressed;
  }
}

export class Tick extends $CustomType {}

export class DelayedMessage extends $CustomType {}

export class TimeoutTriggered extends $CustomType {}

export class IntervalCreated extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class IntervalTick extends $CustomType {}

export class TweenUpdate extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class TweenComplete extends $CustomType {}

export class FullscreenSuccess extends $CustomType {}

export class FullscreenError extends $CustomType {}

export class PointerLockSuccess extends $CustomType {}

export class PointerLockError extends $CustomType {}

export class ClipboardWriteSuccess extends $CustomType {}

export class ClipboardWriteError extends $CustomType {}

export class ClipboardRead extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class ClipboardReadError extends $CustomType {}

function init(_) {
  let model = new Model(
    0.0,
    1.0,
    0x3498db,
    0.0,
    false,
    new $effect.Linear(),
    new $option.None(),
    0,
    "Press keys to trigger effects! [Space] Interval, [S] Stop, [1-7] Tweens, [F] Fullscreen, [L] Lock, [V] Vibrate, [C] Copy, [P] Paste",
    false,
    false,
    "Hello from Tiramisu!",
    new KeyPressState(
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
    ),
  );
  let welcome_effect = $effect.delay(2000, new DelayedMessage());
  return [
    model,
    $effect.batch(toList([$effect.tick(new Tick()), welcome_effect])),
    new $option.None(),
  ];
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
      578,
      "view",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 15132,
        end: 15220,
        pattern_start: 15143,
        pattern_end: 15150
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
      582,
      "view",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $1,
        start: 15265,
        end: 15349,
        pattern_start: 15276,
        pattern_end: 15293
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
      585,
      "view",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $2,
        start: 15353,
        end: 15543,
        pattern_start: 15364,
        pattern_end: 15381
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
      593,
      "view",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $3,
        start: 15569,
        end: 15642,
        pattern_start: 15580,
        pattern_end: 15598
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
      595,
      "view",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $4,
        start: 15646,
        end: 15829,
        pattern_start: 15657,
        pattern_end: 15675
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
      603,
      "view",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $5,
        start: 15845,
        end: 15916,
        pattern_start: 15856,
        pattern_end: 15867
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
      604,
      "view",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $6,
        start: 15919,
        end: 16002,
        pattern_start: 15930,
        pattern_end: 15945
      }
    )
  }
  return toList([
    new $scene.Camera(
      "main",
      cam,
      $transform.at(new $vec3.Vec3(0.0, 2.0, 5.0)),
      new $option.Some(new $vec3.Vec3(0.0, 0.0, 0.0)),
      true,
      new $option.None(),
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

/**
 * Helper to set status message and log it to console
 * 
 * @ignore
 */
function set_status(model, message) {
  $io.println("[Effects Demo] " + message);
  return new Model(
    model.rotation,
    model.cube_scale,
    model.cube_color,
    model.tween_value,
    model.tween_running,
    model.current_easing,
    model.interval_id,
    model.interval_count,
    message,
    model.is_fullscreen,
    model.is_pointer_locked,
    model.clipboard_text,
    model.last_keys,
  );
}

function start_tween(model, easing, effects) {
  let tween_effect = $effect.tween(
    0.5,
    2.0,
    2000,
    easing,
    (var0) => { return new TweenUpdate(var0); },
    new TweenComplete(),
  );
  let easing_name = easing_to_string(easing);
  let updated_model = set_status(
    new Model(
      model.rotation,
      model.cube_scale,
      model.cube_color,
      model.tween_value,
      true,
      easing,
      model.interval_id,
      model.interval_count,
      model.status_message,
      model.is_fullscreen,
      model.is_pointer_locked,
      model.clipboard_text,
      model.last_keys,
    ),
    ("Tween started with " + easing_name) + " easing",
  );
  return [updated_model, listPrepend(tween_effect, effects)];
}

function handle_input(model, input_state) {
  let space_pressed = $input.is_key_pressed(input_state, new $input.Space());
  let s_pressed = $input.is_key_pressed(input_state, new $input.KeyS());
  let digit1_pressed = $input.is_key_pressed(input_state, new $input.Digit1());
  let digit2_pressed = $input.is_key_pressed(input_state, new $input.Digit2());
  let digit3_pressed = $input.is_key_pressed(input_state, new $input.Digit3());
  let digit4_pressed = $input.is_key_pressed(input_state, new $input.Digit4());
  let digit5_pressed = $input.is_key_pressed(input_state, new $input.Digit5());
  let digit6_pressed = $input.is_key_pressed(input_state, new $input.Digit6());
  let digit7_pressed = $input.is_key_pressed(input_state, new $input.Digit7());
  let t_pressed = $input.is_key_pressed(input_state, new $input.KeyT());
  let f_pressed = $input.is_key_pressed(input_state, new $input.KeyF());
  let l_pressed = $input.is_key_pressed(input_state, new $input.KeyL());
  let touched = $input.touches_just_started(input_state);
  let c_pressed = $input.is_key_pressed(input_state, new $input.KeyC());
  let p_pressed = $input.is_key_pressed(input_state, new $input.KeyP());
  let _block;
  let $1 = space_pressed && !model.last_keys.space;
  let $2 = model.interval_id;
  if ($2 instanceof $option.None && $1) {
    let interval_effect = $effect.interval(
      1000,
      new IntervalTick(),
      (var0) => { return new IntervalCreated(var0); },
    );
    let updated_model = set_status(
      new Model(
        model.rotation,
        model.cube_scale,
        model.cube_color,
        model.tween_value,
        model.tween_running,
        model.current_easing,
        model.interval_id,
        0,
        model.status_message,
        model.is_fullscreen,
        model.is_pointer_locked,
        model.clipboard_text,
        model.last_keys,
      ),
      "Interval started - cube will pulse every second",
    );
    _block = [updated_model, toList([interval_effect])];
  } else {
    _block = [model, toList([])];
  }
  let $ = _block;
  let new_model;
  let effects;
  new_model = $[0];
  effects = $[1];
  let _block$1;
  let $4 = s_pressed && !new_model.last_keys.s;
  let $5 = new_model.interval_id;
  if ($5 instanceof $option.Some && $4) {
    let id = $5[0];
    let cancel_effect = $effect.cancel_interval(id);
    let updated_model = set_status(
      new Model(
        new_model.rotation,
        new_model.cube_scale,
        new_model.cube_color,
        new_model.tween_value,
        new_model.tween_running,
        new_model.current_easing,
        new $option.None(),
        new_model.interval_count,
        new_model.status_message,
        new_model.is_fullscreen,
        new_model.is_pointer_locked,
        new_model.clipboard_text,
        new_model.last_keys,
      ),
      "Interval stopped",
    );
    _block$1 = [updated_model, listPrepend(cancel_effect, effects)];
  } else {
    _block$1 = [new_model, effects];
  }
  let $3 = _block$1;
  let new_model$1;
  let mut_effects;
  new_model$1 = $3[0];
  mut_effects = $3[1];
  let _block$2;
  let $7 = digit1_pressed && !new_model$1.last_keys.digit1;
  let $8 = new_model$1.tween_running;
  if (!$8 && $7) {
    _block$2 = start_tween(new_model$1, new $effect.Linear(), mut_effects);
  } else {
    _block$2 = [new_model$1, mut_effects];
  }
  let $6 = _block$2;
  let new_model$2;
  let mut_effects$1;
  new_model$2 = $6[0];
  mut_effects$1 = $6[1];
  let _block$3;
  let $10 = digit2_pressed && !new_model$2.last_keys.digit2;
  let $11 = new_model$2.tween_running;
  if (!$11 && $10) {
    _block$3 = start_tween(new_model$2, new $effect.EaseInQuad(), mut_effects$1);
  } else {
    _block$3 = [new_model$2, mut_effects$1];
  }
  let $9 = _block$3;
  let new_model$3;
  let mut_effects$2;
  new_model$3 = $9[0];
  mut_effects$2 = $9[1];
  let _block$4;
  let $13 = digit3_pressed && !new_model$3.last_keys.digit3;
  let $14 = new_model$3.tween_running;
  if (!$14 && $13) {
    _block$4 = start_tween(
      new_model$3,
      new $effect.EaseOutQuad(),
      mut_effects$2,
    );
  } else {
    _block$4 = [new_model$3, mut_effects$2];
  }
  let $12 = _block$4;
  let new_model$4;
  let mut_effects$3;
  new_model$4 = $12[0];
  mut_effects$3 = $12[1];
  let _block$5;
  let $16 = digit4_pressed && !new_model$4.last_keys.digit4;
  let $17 = new_model$4.tween_running;
  if (!$17 && $16) {
    _block$5 = start_tween(
      new_model$4,
      new $effect.EaseInOutQuad(),
      mut_effects$3,
    );
  } else {
    _block$5 = [new_model$4, mut_effects$3];
  }
  let $15 = _block$5;
  let new_model$5;
  let mut_effects$4;
  new_model$5 = $15[0];
  mut_effects$4 = $15[1];
  let _block$6;
  let $19 = digit5_pressed && !new_model$5.last_keys.digit5;
  let $20 = new_model$5.tween_running;
  if (!$20 && $19) {
    _block$6 = start_tween(
      new_model$5,
      new $effect.EaseInCubic(),
      mut_effects$4,
    );
  } else {
    _block$6 = [new_model$5, mut_effects$4];
  }
  let $18 = _block$6;
  let new_model$6;
  let mut_effects$5;
  new_model$6 = $18[0];
  mut_effects$5 = $18[1];
  let _block$7;
  let $22 = digit6_pressed && !new_model$6.last_keys.digit6;
  let $23 = new_model$6.tween_running;
  if (!$23 && $22) {
    _block$7 = start_tween(
      new_model$6,
      new $effect.EaseOutCubic(),
      mut_effects$5,
    );
  } else {
    _block$7 = [new_model$6, mut_effects$5];
  }
  let $21 = _block$7;
  let new_model$7;
  let mut_effects$6;
  new_model$7 = $21[0];
  mut_effects$6 = $21[1];
  let _block$8;
  let $25 = digit7_pressed && !new_model$7.last_keys.digit7;
  let $26 = new_model$7.tween_running;
  if (!$26 && $25) {
    _block$8 = start_tween(
      new_model$7,
      new $effect.EaseInOutCubic(),
      mut_effects$6,
    );
  } else {
    _block$8 = [new_model$7, mut_effects$6];
  }
  let $24 = _block$8;
  let new_model$8;
  let mut_effects$7;
  new_model$8 = $24[0];
  mut_effects$7 = $24[1];
  let _block$9;
  let $28 = t_pressed && !new_model$8.last_keys.t;
  if ($28) {
    let timeout_effect = $effect.delay(3000, new TimeoutTriggered());
    let updated_model = set_status(new_model$8, "Timeout set");
    _block$9 = [updated_model, listPrepend(timeout_effect, mut_effects$7)];
  } else {
    _block$9 = [new_model$8, mut_effects$7];
  }
  let $27 = _block$9;
  let new_model$9;
  let mut_effects$8;
  new_model$9 = $27[0];
  mut_effects$8 = $27[1];
  let _block$10;
  let $30 = f_pressed && !new_model$9.last_keys.f;
  if ($30) {
    let $31 = new_model$9.is_fullscreen;
    if ($31) {
      let updated_model = set_status(
        new Model(
          new_model$9.rotation,
          new_model$9.cube_scale,
          new_model$9.cube_color,
          new_model$9.tween_value,
          new_model$9.tween_running,
          new_model$9.current_easing,
          new_model$9.interval_id,
          new_model$9.interval_count,
          new_model$9.status_message,
          false,
          new_model$9.is_pointer_locked,
          new_model$9.clipboard_text,
          new_model$9.last_keys,
        ),
        "Exited fullscreen",
      );
      _block$10 = [
        updated_model,
        listPrepend($effect.exit_fullscreen(), mut_effects$8),
      ];
    } else {
      let fullscreen_effect = $effect.request_fullscreen(
        new FullscreenSuccess(),
        new FullscreenError(),
      );
      _block$10 = [new_model$9, listPrepend(fullscreen_effect, mut_effects$8)];
    }
  } else {
    _block$10 = [new_model$9, mut_effects$8];
  }
  let $29 = _block$10;
  let new_model$10;
  let mut_effects$9;
  new_model$10 = $29[0];
  mut_effects$9 = $29[1];
  let _block$11;
  let $32 = l_pressed && !new_model$10.last_keys.l;
  if ($32) {
    let $33 = new_model$10.is_pointer_locked;
    if ($33) {
      let updated_model = set_status(
        new Model(
          new_model$10.rotation,
          new_model$10.cube_scale,
          new_model$10.cube_color,
          new_model$10.tween_value,
          new_model$10.tween_running,
          new_model$10.current_easing,
          new_model$10.interval_id,
          new_model$10.interval_count,
          new_model$10.status_message,
          new_model$10.is_fullscreen,
          false,
          new_model$10.clipboard_text,
          new_model$10.last_keys,
        ),
        "Exited pointer lock",
      );
      _block$11 = [
        updated_model,
        listPrepend($effect.exit_pointer_lock(), mut_effects$9),
      ];
    } else {
      let pointer_lock_effect = $effect.request_pointer_lock(
        new PointerLockSuccess(),
        new PointerLockError(),
      );
      _block$11 = [
        new_model$10,
        listPrepend(pointer_lock_effect, mut_effects$9),
      ];
    }
  } else {
    _block$11 = [new_model$10, mut_effects$9];
  }
  let $31 = _block$11;
  let new_model$11;
  let mut_effects$10;
  new_model$11 = $31[0];
  mut_effects$10 = $31[1];
  let _block$12;
  let $34 = !$list.is_empty(touched) && !new_model$11.last_keys.touch_screen_pressed;
  if ($34) {
    let vibration_effect = $effect.vibrate(toList([200, 100, 200]));
    let updated_model = set_status(
      new_model$11,
      "Vibration triggered (mobile only)",
    );
    _block$12 = [updated_model, listPrepend(vibration_effect, mut_effects$10)];
  } else {
    _block$12 = [new_model$11, mut_effects$10];
  }
  let $33 = _block$12;
  let new_model$12;
  let mut_effects$11;
  new_model$12 = $33[0];
  mut_effects$11 = $33[1];
  let _block$13;
  let $36 = c_pressed && !new_model$12.last_keys.c;
  if ($36) {
    let copy_effect = $effect.clipboard_write(
      new_model$12.clipboard_text,
      new ClipboardWriteSuccess(),
      new ClipboardWriteError(),
    );
    _block$13 = [new_model$12, listPrepend(copy_effect, mut_effects$11)];
  } else {
    _block$13 = [new_model$12, mut_effects$11];
  }
  let $35 = _block$13;
  let new_model$13;
  let mut_effects$12;
  new_model$13 = $35[0];
  mut_effects$12 = $35[1];
  let _block$14;
  let $38 = p_pressed && !new_model$13.last_keys.p;
  if ($38) {
    let paste_effect = $effect.clipboard_read(
      (var0) => { return new ClipboardRead(var0); },
      new ClipboardReadError(),
    );
    _block$14 = [new_model$13, listPrepend(paste_effect, mut_effects$12)];
  } else {
    _block$14 = [new_model$13, mut_effects$12];
  }
  let $37 = _block$14;
  let new_model$14;
  let mut_effects$13;
  new_model$14 = $37[0];
  mut_effects$13 = $37[1];
  let new_keys = new KeyPressState(
    space_pressed,
    s_pressed,
    digit1_pressed,
    digit2_pressed,
    digit3_pressed,
    digit4_pressed,
    digit5_pressed,
    digit6_pressed,
    digit7_pressed,
    t_pressed,
    f_pressed,
    l_pressed,
    c_pressed,
    p_pressed,
    !$list.is_empty(touched),
  );
  let final_model = new Model(
    new_model$14.rotation,
    new_model$14.cube_scale,
    new_model$14.cube_color,
    new_model$14.tween_value,
    new_model$14.tween_running,
    new_model$14.current_easing,
    new_model$14.interval_id,
    new_model$14.interval_count,
    new_model$14.status_message,
    new_model$14.is_fullscreen,
    new_model$14.is_pointer_locked,
    new_model$14.clipboard_text,
    new_keys,
  );
  return [final_model, $effect.batch(mut_effects$13)];
}

function update(model, msg, ctx) {
  if (msg instanceof Tick) {
    let new_rotation = model.rotation + (ctx.delta_time * 0.5);
    let $ = handle_input(model, ctx.input);
    let new_model;
    let key_effects;
    new_model = $[0];
    key_effects = $[1];
    let updated_model = new Model(
      new_rotation,
      new_model.cube_scale,
      new_model.cube_color,
      new_model.tween_value,
      new_model.tween_running,
      new_model.current_easing,
      new_model.interval_id,
      new_model.interval_count,
      new_model.status_message,
      new_model.is_fullscreen,
      new_model.is_pointer_locked,
      new_model.clipboard_text,
      new_model.last_keys,
    );
    return [
      updated_model,
      $effect.batch(toList([$effect.tick(new Tick()), key_effects])),
      new $option.None(),
    ];
  } else if (msg instanceof DelayedMessage) {
    return [
      set_status(model, "Delayed message received after 2 seconds!"),
      $effect.none(),
      new $option.None(),
    ];
  } else if (msg instanceof TimeoutTriggered) {
    return [
      set_status(model, "Timeout triggered!"),
      $effect.none(),
      new $option.None(),
    ];
  } else if (msg instanceof IntervalCreated) {
    let id = msg[0];
    return [
      new Model(
        model.rotation,
        model.cube_scale,
        model.cube_color,
        model.tween_value,
        model.tween_running,
        model.current_easing,
        new $option.Some(id),
        model.interval_count,
        model.status_message,
        model.is_fullscreen,
        model.is_pointer_locked,
        model.clipboard_text,
        model.last_keys,
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
      set_status(
        new Model(
          model.rotation,
          model.cube_scale,
          new_color,
          model.tween_value,
          model.tween_running,
          model.current_easing,
          model.interval_id,
          new_count,
          model.status_message,
          model.is_fullscreen,
          model.is_pointer_locked,
          model.clipboard_text,
          model.last_keys,
        ),
        "Interval tick #" + $int.to_string(new_count),
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
        model.interval_id,
        model.interval_count,
        model.status_message,
        model.is_fullscreen,
        model.is_pointer_locked,
        model.clipboard_text,
        model.last_keys,
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
      set_status(
        new Model(
          model.rotation,
          model.cube_scale,
          model.cube_color,
          model.tween_value,
          false,
          model.current_easing,
          model.interval_id,
          model.interval_count,
          model.status_message,
          model.is_fullscreen,
          model.is_pointer_locked,
          model.clipboard_text,
          model.last_keys,
        ),
        "Tween complete!",
      ),
      reverse_tween,
      new $option.None(),
    ];
  } else if (msg instanceof FullscreenSuccess) {
    return [
      set_status(
        new Model(
          model.rotation,
          model.cube_scale,
          model.cube_color,
          model.tween_value,
          model.tween_running,
          model.current_easing,
          model.interval_id,
          model.interval_count,
          model.status_message,
          true,
          model.is_pointer_locked,
          model.clipboard_text,
          model.last_keys,
        ),
        "Entered fullscreen mode!",
      ),
      $effect.none(),
      new $option.None(),
    ];
  } else if (msg instanceof FullscreenError) {
    return [
      set_status(model, "Fullscreen request failed"),
      $effect.none(),
      new $option.None(),
    ];
  } else if (msg instanceof PointerLockSuccess) {
    return [
      set_status(
        new Model(
          model.rotation,
          model.cube_scale,
          model.cube_color,
          model.tween_value,
          model.tween_running,
          model.current_easing,
          model.interval_id,
          model.interval_count,
          model.status_message,
          model.is_fullscreen,
          true,
          model.clipboard_text,
          model.last_keys,
        ),
        "Pointer locked! Press ESC to exit.",
      ),
      $effect.none(),
      new $option.None(),
    ];
  } else if (msg instanceof PointerLockError) {
    return [
      set_status(model, "Pointer lock request failed"),
      $effect.none(),
      new $option.None(),
    ];
  } else if (msg instanceof ClipboardWriteSuccess) {
    return [
      set_status(
        model,
        ("Copied to clipboard: \"" + model.clipboard_text) + "\"",
      ),
      $effect.none(),
      new $option.None(),
    ];
  } else if (msg instanceof ClipboardWriteError) {
    return [
      set_status(model, "Failed to copy to clipboard"),
      $effect.none(),
      new $option.None(),
    ];
  } else if (msg instanceof ClipboardRead) {
    let text = msg[0];
    return [
      set_status(
        new Model(
          model.rotation,
          model.cube_scale,
          model.cube_color,
          model.tween_value,
          model.tween_running,
          model.current_easing,
          model.interval_id,
          model.interval_count,
          model.status_message,
          model.is_fullscreen,
          model.is_pointer_locked,
          text,
          model.last_keys,
        ),
        ("Pasted from clipboard: \"" + text) + "\"",
      ),
      $effect.none(),
      new $option.None(),
    ];
  } else {
    return [
      set_status(model, "Failed to read from clipboard"),
      $effect.none(),
      new $option.None(),
    ];
  }
}

export function main() {
  return $tiramisu.run(
    new $option.None(),
    new $background.Color(0x1a1a2e),
    init,
    update,
    view,
  );
}
