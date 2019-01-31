open Js;

let pi = Math._PI;

/* Sine */

let easeInSine = t => 1. -. cos(t *. (pi /. 2.));

let easeOutSine = t => sin(t *. (pi /. 2.));

let easeInOutSine = t => {
  let t = t *. 2.;
  t < 1. ? 0.5 *. easeInSine(t) : 0.5 *. easeOutSine(t -. 1.) +. 0.5;
};

let easeOutInSine = t => {
  let t = t *. 2.;
  t < 1. ? 0.5 *. easeOutSine(t) : 0.5 *. easeInSine(t -. 1.) +. 0.5;
};

/* Quad */

let easeInQuad = t => t *. t;

let easeOutQuad = t => {
  let t = 1. -. t;
  1. -. t *. t;
};

let easeInOutQuad = t => {
  let t = t *. 2.;
  t < 1. ? 0.5 *. easeInQuad(t) : 0.5 *. easeOutQuad(t -. 1.) +. 0.5;
};

let easeOutInQuad = t => {
  let t = t *. 2.;
  t < 1. ? 0.5 *. easeOutQuad(t) : 0.5 *. easeInQuad(t -. 1.) +. 0.5;
};

/* Cubic */

let easeInCubic = t => t *. t *. t;

let easeOutCubic = t => {
  let t = 1. -. t;
  1. -. t *. t *. t;
};

let easeInOutCubic = t => {
  let t = t *. 2.;
  t < 1. ? 0.5 *. easeInCubic(t) : 0.5 *. easeOutCubic(t -. 1.) +. 0.5;
};

let easeOutInCubic = t => {
  let t = t *. 2.;
  t < 1. ? 0.5 *. easeOutCubic(t) : 0.5 *. easeInCubic(t -. 1.) +. 0.5;
};

/* Quart */

let easeInQuart = t => t *. t *. t *. t;

let easeOutQuart = t => {
  let t = 1. -. t;
  1. -. t *. t *. t *. t;
};

let easeInOutQuart = t => {
  let t = t *. 2.;
  t < 1. ? 0.5 *. easeInQuart(t) : 0.5 *. easeOutQuart(t -. 1.) +. 0.5;
};

let easeOutInQuart = t => {
  let t = t *. 2.;
  t < 1. ? 0.5 *. easeOutQuart(t) : 0.5 *. easeInQuart(t -. 1.) +. 0.5;
};

/* Quint */

let easeInQuint = t => t *. t *. t *. t *. t;

let easeOutQuint = t => {
  let t = 1. -. t;
  1. -. t *. t *. t *. t *. t;
};

let easeInOutQuint = t => {
  let t = t *. 2.;
  t < 1. ? 0.5 *. easeInQuint(t) : 0.5 *. easeOutQuint(t -. 1.) +. 0.5;
};

let easeOutInQuint = t => {
  let t = t *. 2.;
  t < 1. ? 0.5 *. easeOutQuint(t) : 0.5 *. easeInQuint(t -. 1.) +. 0.5;
};

/* Expo */

let easeInExpo = t => Math.pow_float(~base=2., ~exp=10. *. (t -. 1.));

let easeOutExpo = t => 1. -. Math.pow_float(~base=2., ~exp=(-10.) *. t);

let easeInOutExpo = t => {
  let t = t *. 2.;
  t < 1. ? 0.5 *. easeInExpo(t) : 0.5 *. easeOutExpo(t -. 1.) +. 0.5;
};

let easeOutInExpo = t => {
  let t = t *. 2.;
  t < 1. ? 0.5 *. easeOutExpo(t) : 0.5 *. easeInExpo(t -. 1.) +. 0.5;
};

/* Circ */

let easeInCirc = t => 1. -. sqrt(1. -. t *. t);

let easeOutCirc = t => {
  let t = 1. -. t;
  sqrt(1. -. t *. t);
};

let easeInOutCirc = t => {
  let t = t *. 2.;
  t < 1. ? 0.5 *. easeInCirc(t) : 0.5 *. easeOutCirc(t -. 1.) +. 0.5;
};

let easeOutInCirc = t => {
  let t = t *. 2.;
  t < 1. ? 0.5 *. easeOutCirc(t) : 0.5 *. easeInCirc(t -. 1.) +. 0.5;
};

/* Back */

let easeInBack = t => t *. t *. ((1.70158 +. 1.) *. t -. 1.70158);

let easeOutBack = t => {
  let t = t -. 1.;
  t *. t *. ((1.70158 +. 1.) *. t +. 1.70158) +. 1.;
};

let easeInOutBack = t => {
  let t = t *. 2.;
  t < 1. ? 0.5 *. easeInBack(t) : 0.5 *. easeOutBack(t -. 1.) +. 0.5;
};

let easeOutInBack = t => {
  let t = t *. 2.;
  t < 1. ? 0.5 *. easeOutBack(t) : 0.5 *. easeInBack(t -. 1.) +. 0.5;
};

/* Elastic */

let easeInElastic = t => {
  let t = t -. 1.;
  -. Math.pow_float(~base=2., ~exp=10. *. t)
  *. sin((t -. 0.3 /. 4.) *. (2. *. pi) /. 0.3);
};

let easeOutElastic = t =>
  Math.pow_float(~base=2., ~exp=(-10.) *. t)
  *. sin((t -. 0.3 /. 4.) *. (2. *. pi) /. 0.3)
  +. 1.;

let easeInOutElastic = t => {
  let t = t *. 2.;
  t < 1. ? 0.5 *. easeInElastic(t) : 0.5 *. easeOutElastic(t -. 1.) +. 0.5;
};

let easeOutInElastic = t => {
  let t = t *. 2.;
  t < 1. ? 0.5 *. easeOutElastic(t) : 0.5 *. easeInElastic(t -. 1.) +. 0.5;
};

/* Bounce */

let easeOutBounce = t => {
  switch (t) {
  | n when n < 1. /. 2.75 => 7.5625 *. t *. t
  | n when n < 2. /. 2.75 =>
    let n = n -. 1.5 /. 2.75;
    7.5625 *. n *. n +. 0.75;
  | n when n < 2.5 /. 2.75 =>
    let n = n -. 2.25 /. 2.75;
    7.5625 *. n *. n +. 0.9375;
  | n =>
    let n = n -. 2.625 /. 2.75;
    7.5625 *. n *. n +. 0.984375;
  };
};

let easeInBounce = t => 1. -. easeOutBounce(1. -. t);

let easeInOutBounce = t => {
  let t = t *. 2.;
  t < 1. ? 0.5 *. easeInBounce(t) : 0.5 *. easeOutBounce(t -. 1.) +. 0.5;
};

let easeOutInBounce = t => {
  let t = t *. 2.;
  t < 1. ? 0.5 *. easeOutBounce(t) : 0.5 *. easeInBounce(t -. 1.) +. 0.5;
};

/* Linear */

let linear = t => t;
