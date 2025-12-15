const pow = Math.pow;
const sqrt = Math.sqrt;
const sin = Math.sin;
const cos = Math.cos;
const PI = Math.PI;
const c1 = 1.70158;
const c2 = c1 * 1.525;
const c3 = c1 + 1;
const c4 = (2 * PI) / 3;
const c5 = (2 * PI) / 4.5;

const bounceOut = function (x) {
  const n1 = 7.5625;
  const d1 = 2.75;

  if (x < 1 / d1) {
    return n1 * x * x;
  } else if (x < 2 / d1) {
    return n1 * (x -= 1.5 / d1) * x + 0.75;
  } else if (x < 2.5 / d1) {
    return n1 * (x -= 2.25 / d1) * x + 0.9375;
  } else {
    return n1 * (x -= 2.625 / d1) * x + 0.984375;
  }
};

export const linear = (x) => x;
export function easeInQuad(x) {
  return x * x;
}
export function easeOutQuad(x) {
  return 1 - (1 - x) * (1 - x);
}
export function easeInOutQuad(x) {
  return x < 0.5 ? 2 * x * x : 1 - pow(-2 * x + 2, 2) / 2;
}
export function easeInCubic(x) {
  return x * x * x;
}
export function easeOutCubic(x) {
  return 1 - pow(1 - x, 3);
}
export function easeInOutCubic(x) {
  return x < 0.5 ? 4 * x * x * x : 1 - pow(-2 * x + 2, 3) / 2;
}
export function easeInQuart(x) {
  return x * x * x * x;
}
export function easeOutQuart(x) {
  return 1 - pow(1 - x, 4);
}
export function easeInOutQuart(x) {
  return x < 0.5 ? 8 * x * x * x * x : 1 - pow(-2 * x + 2, 4) / 2;
}
export function easeInQuint(x) {
  return x * x * x * x * x;
}
export function easeOutQuint(x) {
  return 1 - pow(1 - x, 5);
}
export function easeInOutQuint(x) {
  return x < 0.5 ? 16 * x * x * x * x * x : 1 - pow(-2 * x + 2, 5) / 2;
}
export function easeInSine(x) {
  return 1 - cos((x * PI) / 2);
}
export function easeOutSine(x) {
  return sin((x * PI) / 2);
}
export function easeInOutSine(x) {
  return -(cos(PI * x) - 1) / 2;
}
export function easeInExpo(x) {
  return x === 0 ? 0 : pow(2, 10 * x - 10);
}
export function easeOutExpo(x) {
  return x === 1 ? 1 : 1 - pow(2, -10 * x);
}
export function easeInOutExpo(x) {
  return x === 0
    ? 0
    : x === 1
      ? 1
      : x < 0.5
        ? pow(2, 20 * x - 10) / 2
        : (2 - pow(2, -20 * x + 10)) / 2;
}
export function easeInCirc(x) {
  return 1 - sqrt(1 - pow(x, 2));
}
export function easeOutCirc(x) {
  return sqrt(1 - pow(x - 1, 2));
}
export function easeInOutCirc(x) {
  return x < 0.5
    ? (1 - sqrt(1 - pow(2 * x, 2))) / 2
    : (sqrt(1 - pow(-2 * x + 2, 2)) + 1) / 2;
}
export function easeInBack(x) {
  return c3 * x * x * x - c1 * x * x;
}
export function easeOutBack(x) {
  return 1 + c3 * pow(x - 1, 3) + c1 * pow(x - 1, 2);
}
export function easeInOutBack(x) {
  return x < 0.5
    ? (pow(2 * x, 2) * ((c2 + 1) * 2 * x - c2)) / 2
    : (pow(2 * x - 2, 2) * ((c2 + 1) * (x * 2 - 2) + c2) + 2) / 2;
}
export function easeInElastic(x) {
  return x === 0
    ? 0
    : x === 1
      ? 1
      : -pow(2, 10 * x - 10) * sin((x * 10 - 10.75) * c4);
}
export function easeOutElastic(x) {
  return x === 0
    ? 0
    : x === 1
      ? 1
      : pow(2, -10 * x) * sin((x * 10 - 0.75) * c4) + 1;
}
export function easeInOutElastic(x) {
  return x === 0
    ? 0
    : x === 1
      ? 1
      : x < 0.5
        ? -(pow(2, 20 * x - 10) * sin((20 * x - 11.125) * c5)) / 2
        : (pow(2, -20 * x + 10) * sin((20 * x - 11.125) * c5)) / 2 + 1;
}
export function easeInBounce(x) {
  return 1 - bounceOut(1 - x);
}
export const easeOutBounce = bounceOut;
export function easeInOutBounce(x) {
  return x < 0.5
    ? (1 - bounceOut(1 - 2 * x)) / 2
    : (1 + bounceOut(2 * x - 1)) / 2;
}
