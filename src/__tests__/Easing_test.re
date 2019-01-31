open Jest;
open ExpectJs;

type easingFunctionT =
  (float, ~start: float=?, ~final: float=?, ~duration: float=?, unit) => float;

let testCases = (targetFn: easingFunctionT, mode, expectedTable) => {
  let start = 0.;
  let final = 100.;
  let duration = 100.;
  let elapsedTable = [|0., 25., 50., 75., 100.|];

  describe(mode, () =>
    elapsedTable->Belt.Array.forEachWithIndex((index, elapsed) =>
      test({j|t: $elapsed|j}, () =>
        expect(targetFn(elapsed, ~start, ~final, ~duration, ()))
        |> toBeSoCloseTo(~digits=1, expectedTable[index])
      )
    )
  );
};

let () =
  describe("Easing functions", () => {
    describe("sine", () => {
      testCases(
        Easing.easeInSine,
        "in",
        [|
          0.,
          7.612046748871322,
          29.289321881345245,
          61.73165676349102,
          100.,
        |],
      );

      testCases(
        Easing.easeOutSine,
        "out",
        [|
          0.,
          38.268343236508976,
          70.71067811865474,
          92.38795325112868,
          100.,
        |],
      );

      testCases(
        Easing.easeInOutSine,
        "in-out",
        [|0., 14.64466094067262, 50., 85.35533905932738, 100.|],
      );

      testCases(
        Easing.easeOutInSine,
        "out-in",
        [|0., 35.35533905932737, 50., 64.64466094067262, 100.|],
      );
    });

    describe("quad", () => {
      testCases(Easing.easeInQuad, "in", [|0., 6.25, 25., 56.25, 100.|]);

      testCases(Easing.easeOutQuad, "out", [|0., 43.75, 75., 93.75, 100.|]);

      testCases(
        Easing.easeInOutQuad,
        "in-out",
        [|0., 12.5, 50., 87.5, 100.|],
      );

      testCases(
        Easing.easeOutInQuad,
        "out-in",
        [|0., 37.5, 50., 62.5, 100.|],
      );
    });

    describe("cubic", () => {
      testCases(
        Easing.easeInCubic,
        "in",
        [|0., 1.5625, 12.5, 42.1875, 100.|],
      );

      testCases(
        Easing.easeOutCubic,
        "out",
        [|0., 57.8125, 87.5, 98.4375, 100.|],
      );

      testCases(
        Easing.easeInOutCubic,
        "in-out",
        [|0., 6.25, 50., 93.75, 100.|],
      );

      testCases(
        Easing.easeOutInCubic,
        "out-in",
        [|0., 43.75, 50., 56.25, 100.|],
      );
    });

    describe("quart", () => {
      testCases(
        Easing.easeInQuart,
        "in",
        [|0., 0.390625, 6.25, 31.640625, 100.|],
      );

      testCases(
        Easing.easeOutQuart,
        "out",
        [|0., 68.359375, 93.75, 99.609375, 100.|],
      );

      testCases(
        Easing.easeInOutQuart,
        "in-out",
        [|0., 3.125, 50., 96.875, 100.|],
      );

      testCases(
        Easing.easeOutInQuart,
        "out-in",
        [|0., 46.875, 50., 53.125, 100.|],
      );
    });

    describe("quint", () => {
      testCases(
        Easing.easeInQuint,
        "in",
        [|0., 0.9765625, 3.125, 23.73046875, 100.|],
      );

      testCases(
        Easing.easeOutQuint,
        "out",
        [|0., 76.26953125, 96.875, 99.90234375, 100.|],
      );

      testCases(
        Easing.easeInOutQuint,
        "in-out",
        [|0., 1.5625, 50., 98.4375, 100.|],
      );

      testCases(
        Easing.easeOutInQuint,
        "out-in",
        [|0., 48.4375, 50., 51.5625, 100.|],
      );
    });

    describe("expo", () => {
      testCases(
        Easing.easeInExpo,
        "in",
        [|0., 0.5524271728019903, 3.125, 17.67766952966369, 100.|],
      );

      testCases(
        Easing.easeOutExpo,
        "out",
        [|0., 82.32233047033631, 96.875, 99.447572827198, 100.|],
      );

      testCases(
        Easing.easeInOutExpo,
        "in-out",
        [|0., 1.5625, 50., 98.4375, 100.|],
      );

      testCases(
        Easing.easeOutInExpo,
        "out-in",
        [|0., 48.4375, 50., 51.5625, 100.|],
      );
    });

    describe("circ", () => {
      testCases(
        Easing.easeInCirc,
        "in",
        [|
          0.,
          3.1754163448145745,
          13.397459621556141,
          33.85621722338523,
          100.,
        |],
      );

      testCases(
        Easing.easeOutCirc,
        "out",
        [|0., 66.14378277661477, 86.60254037844386, 96.82458365518542, 100.|],
      );

      testCases(
        Easing.easeInOutCirc,
        "in-out",
        [|0., 6.698729810778071, 50., 93.30127018922192, 100.|],
      );

      testCases(
        Easing.easeOutInCirc,
        "out-in",
        [|0., 43.30127018922193, 50., 56.69872981077807, 100.|],
      );
    });

    describe("back", () => {
      testCases(
        Easing.easeInBack,
        "in",
        [|
          (-0.),
          (-6.413656250000001),
          (-8.769750000000004),
          18.259031249999992,
          99.99999999999997,
        |],
      );

      testCases(
        Easing.easeOutBack,
        "out",
        [|
          2.220446049250313e-14,
          81.74096875000001,
          108.76975,
          106.41365625000002,
          100.,
        |],
      );

      testCases(
        Easing.easeInOutBack,
        "in-out",
        [|(-0.), (-4.384875000000002), 50.00000000000014, 104.384875, 100.|],
      );

      testCases(
        Easing.easeOutInBack,
        "out-in",
        [|
          1.1102230246251565e-14,
          54.384875,
          50.,
          45.615125,
          99.99999999999999,
        |],
      );
    });

    describe("elastic", () => {
      testCases(
        Easing.easeInElastic,
        "in",
        [|
          0.,
          (-0.5524271728019903),
          (-1.5625000000000044),
          8.838834764831844,
          100.,
        |],
      );

      testCases(
        Easing.easeOutElastic,
        "out",
        [|0., 91.16116523516816, 101.5625, 100.552427172802, 100.|],
      );

      testCases(
        Easing.easeInOutElastic,
        "in-out",
        [|0., (-0.7812500000000022), 50., 100.78125, 100.|],
      );

      testCases(
        Easing.easeOutInElastic,
        "out-in",
        [|0., 50.78125, 50., 49.21875, 100.|],
      );
    });

    describe("bounce", () => {
      testCases(
        Easing.easeInBounce,
        "in",
        [|0., 2.734375, 23.4375, 52.734375, 100.|],
      );

      testCases(
        Easing.easeOutBounce,
        "out",
        [|0., 47.265625, 76.5625, 97.265625, 100.|],
      );

      testCases(
        Easing.easeInOutBounce,
        "in-out",
        [|0., 11.71875, 50., 88.28125, 100.|],
      );

      testCases(
        Easing.easeOutInBounce,
        "out-in",
        [|0., 38.28125, 50., 61.71875, 100.|],
      );
    });

    describe("linear", () =>
      testCases(Easing.linear, "", [|0., 25., 50., 75., 100.|])
    );
  });
