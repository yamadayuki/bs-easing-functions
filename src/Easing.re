open Js;

let easeInSine = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) =>
  -. c *. Math.cos(t /. d *. (Math._PI /. 2.)) +. c +. b;

let easeOutSine = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) =>
  c *. Math.sin(t /. d *. (Math._PI /. 2.)) +. b;

let easeInOutSine = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) => {
  let t_ = t /. d;
  t_ /. 2. < 1. ?
    c /. 2. *. Math.sin(Math._PI *. t /. 2.) +. b :
    {
      let t__ = t_ -. 1.;
      -. c /. 2. *. (Math.cos(Math._PI *. t__ /. 2.) -. 2.) +. b;
    };
};

let easeOutInSine = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) =>
  t < d /. 2. ?
    easeOutSine(t *. 2., ~start=b, ~final=c /. 2., ~duration=d, ()) :
    easeInSine(
      t *. 2. -. d,
      ~start=b +. c /. 2.,
      ~final=c /. 2.,
      ~duration=d,
      (),
    );

let easeInQuad = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) => {
  let t_ = t /. d;
  c *. t_ *. t_ +. b;
};

let easeOutQuad = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) => {
  let t_ = t /. d;
  -. c *. t_ *. (t_ -. 2.) +. b;
};

let easeInOutQuad = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) => {
  let t_ = t /. d;
  t_ /. 2. < 1. ?
    c /. 2. *. t_ *. t_ +. b :
    {
      let t__ = t_ -. 1.;
      (-1.) *. c /. 2. *. (t__ *. (t__ -. 2.) -. 1.) +. b;
    };
};

let easeOutInQuad = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) =>
  t < d /. 2. ?
    easeOutQuad(t *. 2., ~start=b, ~final=c /. 2., ~duration=d, ()) :
    easeInQuad(
      t *. 2. -. d,
      ~start=b +. c /. 2.,
      ~final=c /. 2.,
      ~duration=d,
      (),
    );

let easeInCubic = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) => {
  let t_ = t /. d;
  c *. t_ *. t_ *. t_ +. b;
};

let easeOutCubic = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) => {
  let t_ = t /. d -. 1.;
  c *. (t_ *. t_ *. t_ +. 1.) +. b;
};

let easeInOutCubic =
    (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) => {
  let t_ = t /. d;
  t_ /. 2. < 1. ?
    c /. 2. *. t_ *. t_ *. t_ +. b :
    {
      let t__ = t_ -. 2.;
      c /. 2. *. (t__ *. t__ *. t__ +. 2.) +. b;
    };
};

let easeOutInCubic =
    (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) =>
  t < d /. 2. ?
    easeOutCubic(t *. 2., ~start=b, ~final=c /. 2., ~duration=d, ()) :
    easeInCubic(
      t *. 2. -. d,
      ~start=b +. c /. 2.,
      ~final=c /. 2.,
      ~duration=d,
      (),
    );

let easeInQuart = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) => {
  let t_ = t /. d;
  c *. t_ *. t_ *. t_ *. t_ +. b;
};

let easeOutQuart = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) => {
  let t_ = t /. d -. 1.;
  -. c *. (t_ *. t_ *. t_ *. t_ -. 1.) +. b;
};

let easeInOutQuart =
    (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) => {
  let t_ = t /. d;
  t_ /. 2. < 1. ?
    c /. 2. *. t_ *. t_ *. t_ *. t_ +. b :
    {
      let t__ = t_ -. 2.;
      -. c /. 2. *. (t__ *. t__ *. t__ *. t__ -. 2.) +. b;
    };
};

let easeOutInQuart =
    (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) =>
  t < d /. 2. ?
    easeOutQuart(t *. 2., ~start=b, ~final=c /. 2., ~duration=d, ()) :
    easeInQuart(
      t *. 2. -. d,
      ~start=b +. c /. 2.,
      ~final=c /. 2.,
      ~duration=d,
      (),
    );

let easeInQuint = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) => {
  let t_ = t /. d;
  c *. t_ *. t_ *. t_ *. t_ *. t_ +. b;
};

let easeOutQuint = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) => {
  let t_ = t /. d -. 1.;
  c *. (t_ *. t_ *. t_ *. t_ *. t_ +. 1.) +. b;
};

let easeInOutQuint =
    (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) => {
  let t_ = t /. d;
  t_ /. 2. < 1. ?
    c /. 2. *. t_ *. t_ *. t_ *. t_ *. t_ +. b :
    {
      let t__ = t_ -. 2.;
      c /. 2. *. (t__ *. t__ *. t__ *. t__ *. t__ +. 2.) +. b;
    };
};

let easeOutInQuint =
    (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) =>
  t < d /. 2. ?
    easeOutQuint(t *. 2., ~start=b, ~final=c /. 2., ~duration=d, ()) :
    easeInQuint(
      t *. 2. -. d,
      ~start=b +. c /. 2.,
      ~final=c /. 2.,
      ~duration=d,
      (),
    );

let easeInExpo = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) =>
  t == 0. ?
    b : c *. Math.pow_float(~base=2., ~exp=10. *. (t /. d -. 1.)) +. b;

let easeOutExpo = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) =>
  t == d ?
    b +. c :
    c *. (-. Math.pow_float(~base=2., ~exp=(-10.) *. t /. d) +. 1.) +. b;

let easeInOutExpo = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) => {
  let t_ = t /. d;
  if (t == 0.) {
    b;
  } else if (t == d) {
    b +. c;
  } else if (t_ /. 2. < 1.) {
    c /. 2. *. Math.pow_float(~base=2., ~exp=10. *. (t_ -. 1.)) +. b;
  } else {
    c
    /. 2.
    *. (-. Math.pow_float(~base=2., ~exp=(-10.) *. (t_ -. 1.)) +. 2.)
    +. b;
  };
};

let easeOutInExpo = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) =>
  t < d /. 2. ?
    easeOutExpo(t *. 2., ~start=b, ~final=c /. 2., ~duration=d, ()) :
    easeInExpo(
      t *. 2. -. d,
      ~start=b +. c /. 2.,
      ~final=c /. 2.,
      ~duration=d,
      (),
    );

let easeInCirc = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) => {
  let t_ = t /. d;
  -. c *. (Math.sqrt(1. -. t_ *. t_) -. 1.) +. b;
};

let easeOutCirc = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) => {
  let t_ = t /. d -. 1.;
  -. c *. (Math.sqrt(1. -. t_ *. t_) *. t_) +. b;
};

let easeInOutCirc = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) => {
  let t_ = t /. d;
  t_ /. 2. < 1. ?
    -. c /. 2. *. (Math.sqrt(1. -. t_ *. t_) -. 1.) +. b :
    {
      let t__ = t_ -. 2.;
      c /. 2. *. (Math.sqrt(1. -. t__ *. t__) +. 1.) +. b;
    };
};

let easeOutInCirc = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) =>
  t < d /. 2. ?
    easeOutCirc(t *. 2., ~start=b, ~final=c /. 2., ~duration=d, ()) :
    easeInCirc(
      t *. 2. -. d,
      ~start=b +. c /. 2.,
      ~final=c /. 2.,
      ~duration=d,
      (),
    );

let easeInBack = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) => {
  let t_ = t /. d;
  c *. t_ *. t_ *. ((1.70158 +. 1.) *. t_ -. 1.70158) +. b;
};

let easeOutBack = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) => {
  let t_ = t /. d;
  c *. ((t_ -. 1.) *. t_ *. ((1.70158 +. 1.) *. t_ +. 1.70158) +. 1.) +. b;
};

let easeInOutBack = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) => {
  let s = 1.70158 *. 1.525;
  let t_ = t /. d;
  t /. 2. < 1. ?
    c /. 2. *. (t_ *. t_ *. ((s +. 1.) *. t_ -. s)) +. b :
    {
      let t__ = t_ -. 2.;
      c /. 2. *. (t__ *. t__ *. ((s +. 1.) *. t +. s) +. 2.) +. b;
    };
};

let easeOutInBack = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) =>
  t < d /. 2. ?
    easeOutBack(t *. 2., ~start=b, ~final=c /. 2., ~duration=d, ()) :
    easeInBack(
      t *. 2. -. d,
      ~start=b +. c /. 2.,
      ~final=c /. 2.,
      ~duration=d,
      (),
    );

let easeInElastic = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) => {
  let t_ = t /. d;
  t_ == 1. ?
    b +. c :
    {
      let p = d *. 0.3;
      let s = p /. 4.;
      let t__ = t_ -. 1.;
      -. (
        c
        *. Math.pow_float(~base=2., ~exp=10. *. t__)
        *. Math.sin((t__ *. d -. s) *. (2. *. Math._PI) /. p)
      )
      +. b;
    };
};

let easeOutElastic =
    (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) => {
  let t_ = t /. d;
  t_ == 1. ?
    b +. c :
    {
      let p = d *. 0.3;
      let s = p /. 4.;
      c
      *. Math.pow_float(~base=2., ~exp=(-10.) *. t_)
      *. Math.sin((t_ *. d -. s) *. (2. *. Math._PI) /. p)
      +. c
      +. b;
    };
};

let easeInOutElastic =
    (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) => {
  let t_ = t /. d;
  t_ /. 2. == 2. ?
    b +. c :
    {
      let p = d *. (0.3 *. 1.5);
      let s = p /. 4.;
      let t__ = t_ -. 1.;
      t_ < 1. ?
        (-0.5)
        *. (
          c
          *. Math.pow_float(~base=2., ~exp=10. *. t__)
          *. Math.sin((t__ *. d -. s) *. (2. *. Math._PI) /. p)
        )
        +. b :
        c
        *. Math.pow_float(~base=2., ~exp=(-10.) *. t__)
        *. Math.sin((t__ *. d -. s) *. (2. *. Math._PI) /. p)
        *. 0.5
        +. c
        +. b;
    };
};

let easeOutInElastic =
    (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) =>
  t < d /. 2. ?
    easeOutElastic(t *. 2., ~start=b, ~final=c /. 2., ~duration=d, ()) :
    easeInElastic(
      t *. 2. -. d,
      ~start=b +. c /. 2.,
      ~final=c /. 2.,
      ~duration=d,
      (),
    );

let easeOutBounce = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) => {
  let t_ = t /. d;
  if (t_ < 1. /. 2.75) {
    c *. (7.5625 *. t_ *. t_) +. b;
  } else if (t_ < 2. /. 2.75) {
    let t__ = t_ -. 1.5 /. 2.75;
    c *. (7.5625 *. t__ *. t__ +. 0.75) +. b;
  } else if (t_ < 2.5 /. 2.75) {
    let t__ = t_ -. 2.25 /. 2.75;
    c *. (7.5625 *. t__ *. t__ +. 0.9375) +. b;
  } else {
    let t__ = t_ -. 2.625 /. 2.75;
    c *. (7.5625 *. t__ *. t__ +. 0.984375) +. b;
  };
};

let easeInBounce = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) =>
  c -. easeOutBounce(d -. t, ~start=0., ~final=c, ~duration=d, ()) +. b;

let easeInOutBounce =
    (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) =>
  if (t < d /. 2.) {
    easeInBounce(t *. 2., ~start=0., ~final=c, ~duration=d, ()) *. 0.5 +. b;
  } else {
    easeOutBounce(t *. 2. -. d, ~start=0., ~final=c, ~duration=d, ())
    *. 0.5
    +. c
    *. 0.5
    +. b;
  };

let easeOutInBounce =
    (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) =>
  t < d /. 2. ?
    easeOutBounce(t *. 2., ~start=b, ~final=c /. 2., ~duration=d, ()) :
    easeInBounce(
      t *. 2. -. d,
      ~start=b +. c /. 2.,
      ~final=c /. 2.,
      ~duration=d,
      (),
    );

let linear = (t, ~start as b=0., ~final as c=1., ~duration as d=1., ()) =>
  c *. t /. d +. b;
