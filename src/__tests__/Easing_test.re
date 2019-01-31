open Jest;
open ExpectJs;

type expected = (float => float, array(float));

type expectedTable = {
  in_: expected,
  out: expected,
  inOut: expected,
  outIn: expected,
};

let testCases = (expectedTable: expectedTable) => {
  let elapsedTable = [|0., 0.25, 0.5, 0.75, 1.|];

  let {in_, out, inOut, outIn} = expectedTable;

  describe("in", () =>
    elapsedTable->Belt.Array.forEachWithIndex((index, elapsed) =>
      test({j|t: $elapsed|j}, () => {
        let (fn, table) = in_;
        expect(fn(elapsed)) |> toBeCloseTo(table[index]);
      })
    )
  );

  describe("out", () =>
    elapsedTable->Belt.Array.forEachWithIndex((index, elapsed) =>
      test({j|t: $elapsed|j}, () => {
        let (fn, table) = out;
        expect(fn(elapsed)) |> toBeCloseTo(table[index]);
      })
    )
  );

  describe("in-out", () =>
    elapsedTable->Belt.Array.forEachWithIndex((index, elapsed) =>
      test({j|t: $elapsed|j}, () => {
        let (fn, table) = inOut;
        expect(fn(elapsed)) |> toBeCloseTo(table[index]);
      })
    )
  );

  describe("out-in", () =>
    elapsedTable->Belt.Array.forEachWithIndex((index, elapsed) =>
      test({j|t: $elapsed|j}, () => {
        let (fn, table) = outIn;
        expect(fn(elapsed)) |> toBeCloseTo(table[index]);
      })
    )
  );
};

let () =
  describe("Easing functions", () => {
    describe("sine", () =>
      testCases({
        in_: (
          Easing.easeInSine,
          [|
            0.0,
            0.07612046748871326,
            0.2928932188134524,
            0.6173165676349102,
            1.0,
          |],
        ),
        out: (
          Easing.easeOutSine,
          [|
            0.0,
            0.3826834323650898,
            0.7071067811865475,
            0.9238795325112867,
            1.0,
          |],
        ),
        inOut: (
          Easing.easeInOutSine,
          [|0.0, 0.1464466094067262, 0.5, 0.8535533905932737, 1.0|],
        ),
        outIn: (
          Easing.easeOutInSine,
          [|0.0, 0.35355339059327373, 0.5, 0.6464466094067263, 1.0|],
        ),
      })
    );

    describe("quad", () =>
      testCases({
        in_: (Easing.easeInQuad, [|0.0, 0.0625, 0.25, 0.5625, 1.0|]),
        out: (Easing.easeOutQuad, [|0.0, 0.4375, 0.75, 0.9375, 1.0|]),
        inOut: (Easing.easeInOutQuad, [|0.0, 0.125, 0.5, 0.875, 1.0|]),
        outIn: (Easing.easeOutInQuad, [|0.0, 0.375, 0.5, 0.625, 1.0|]),
      })
    );

    describe("cubic", () =>
      testCases({
        in_: (Easing.easeInCubic, [|0.0, 0.015625, 0.125, 0.421875, 1.0|]),
        out: (Easing.easeOutCubic, [|0.0, 0.578125, 0.875, 0.984375, 1.0|]),
        inOut: (Easing.easeInOutCubic, [|0.0, 0.0625, 0.5, 0.9375, 1.0|]),
        outIn: (Easing.easeOutInCubic, [|0.0, 0.4375, 0.5, 0.5625, 1.0|]),
      })
    );

    describe("quart", () =>
      testCases({
        in_: (
          Easing.easeInQuart,
          [|0.0, 0.00390625, 0.0625, 0.31640625, 1.0|],
        ),
        out: (
          Easing.easeOutQuart,
          [|0.0, 0.68359375, 0.9375, 0.99609375, 1.0|],
        ),
        inOut: (Easing.easeInOutQuart, [|0.0, 0.03125, 0.5, 0.96875, 1.0|]),
        outIn: (Easing.easeOutInQuart, [|0.0, 0.46875, 0.5, 0.53125, 1.0|]),
      })
    );

    describe("quint", () =>
      testCases({
        in_: (
          Easing.easeInQuint,
          [|0.0, 0.0009765625, 0.03125, 0.2373046875, 1.0|],
        ),
        out: (
          Easing.easeOutQuint,
          [|0.0, 0.7626953125, 0.96875, 0.9990234375, 1.0|],
        ),
        inOut: (
          Easing.easeInOutQuint,
          [|0.0, 0.015625, 0.5, 0.984375, 1.0|],
        ),
        outIn: (
          Easing.easeOutInQuint,
          [|0.0, 0.484375, 0.5, 0.515625, 1.0|],
        ),
      })
    );

    describe("expo", () =>
      testCases({
        in_: (
          Easing.easeInExpo,
          [|0.0, 0.005524271728019903, 0.03125, 0.1767766952966369, 1.0|],
        ),
        out: (
          Easing.easeOutExpo,
          [|0.0, 0.8232233047033631, 0.96875, 0.99447572827198, 1.0|],
        ),
        inOut: (Easing.easeInOutExpo, [|0.0, 0.015625, 0.5, 0.984375, 1.0|]),
        outIn: (Easing.easeOutInExpo, [|0.0, 0.484375, 0.5, 0.515625, 1.0|]),
      })
    );

    describe("circ", () =>
      testCases({
        in_: (
          Easing.easeInCirc,
          [|
            0.0,
            0.031754163448145745,
            0.1339745962155614,
            0.3385621722338523,
            1.0,
          |],
        ),
        out: (
          Easing.easeOutCirc,
          [|
            0.0,
            0.6614378277661477,
            0.8660254037844386,
            0.9682458365518543,
            1.0,
          |],
        ),
        inOut: (
          Easing.easeInOutCirc,
          [|0.0, 0.0669872981077807, 0.5, 0.9330127018922193, 1.0|],
        ),
        outIn: (
          Easing.easeOutInCirc,
          [|0.0, 0.4330127018922193, 0.5, 0.5669872981077807, 1.0|],
        ),
      })
    );

    describe("back", () =>
      testCases({
        in_: (
          Easing.easeInBack,
          [|
            0.0,
            (-0.06413656250000001),
            (-0.08769750000000004),
            0.1825903124999999,
            1.0,
          |],
        ),
        out: (
          Easing.easeOutBack,
          [|
            2.220446049250313e-16,
            0.8174096875000001,
            1.0876975,
            1.0641365625,
            1.0,
          |],
        ),
        inOut: (
          Easing.easeInOutBack,
          [|
            0.0,
            (-0.04384875000000002),
            0.5000000000000001,
            1.04384875,
            1.0,
          |],
        ),
        outIn: (
          Easing.easeOutInBack,
          [|1.1102230246251565e-16, 0.54384875, 0.5, 0.45615125, 1.0|],
        ),
      })
    );

    describe("elastic", () =>
      testCases({
        in_: (
          Easing.easeInElastic,
          [|
            0.0,
            (-0.005524271728019903),
            (-0.015625000000000045),
            0.08838834764831845,
            1.0,
          |],
        ),
        out: (
          Easing.easeOutElastic,
          [|0.0, 0.9116116523516815, 1.015625, 1.00552427172802, 1.0|],
        ),
        inOut: (
          Easing.easeInOutElastic,
          [|0.0, (-0.007812500000000023), 0.5, 1.0078125, 1.0|],
        ),
        outIn: (
          Easing.easeOutInElastic,
          [|0.0, 0.5078125, 0.5, 0.4921875, 1.0|],
        ),
      })
    );

    describe("bounce", () =>
      testCases({
        in_: (
          Easing.easeInBounce,
          [|0.0, 0.02734375, 0.234375, 0.52734375, 1.0|],
        ),
        out: (
          Easing.easeOutBounce,
          [|0.0, 0.47265625, 0.765625, 0.97265625, 1.0|],
        ),
        inOut: (
          Easing.easeInOutBounce,
          [|0.0, 0.1171875, 0.5, 0.8828125, 1.0|],
        ),
        outIn: (
          Easing.easeOutInBounce,
          [|0.0, 0.3828125, 0.5, 0.6171875, 1.0|],
        ),
      })
    );

    describe("linear", () => {
      let table = [|0.0, 0.25, 0.5, 0.75, 1.0|];

      [|0., 0.25, 0.5, 0.75, 1.|]
      ->Belt.Array.forEachWithIndex((index, elapsed) =>
          test({j|t: $elapsed|j}, () =>
            expect(Easing.linear(elapsed)) |> toBeCloseTo(table[index])
          )
        );
    });
  });
