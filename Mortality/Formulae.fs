namespace Mortality

module Formulae =

    /// Probability that someone of age x will survive to x+n, using an lx table
    let npx (lx : float[]) n x =
        let ω = lx.Length-1
        if x+n >= ω+1 then
            0.
        else
            lx.[x+n] / lx.[x]

    /// Probability that someone of age x will die between ages x and x+n, using an lx table
    let nqx (lx : float[]) n x =
        let ω = lx.Length-1
        if x+n >= ω+1 then
            1.
        else
            (lx.[x] - lx.[x+n]) / lx.[x]

    /// Convert an lx table into a qx table
    let lxToQx (lx : float[]) =
        Array.init (lx.Length-1) (fun x -> nqx lx 1 x)

    /// Convert a qx table into an lx table, given an intial number of lives (conventionally, 100000)
    let qxToLx l0 (qx : float[]) =
        let ω = qx.Length+1
        let lx = Array.zeroCreate ω
        lx.[0] <- l0
        for x in [1..ω-2] do
            lx.[x] <- lx.[x-1] * (1.-qx.[x-1])
        lx.[ω-1] <- 0.
        lx

    /// Curtate life expectancy at age x, using an lx table
    let e (lx : float[]) x =
        let ω = lx.Length-1
        [1..ω-x-1]
        |> Seq.map (fun k -> npx lx k x)
        |> Seq.sum

    /// Curtate life expectancy at age x, using a qx table
    let eq (qx : float[]) x =
        let ω = qx.Length-1
        let rec p i acc =
            if i < 0 then
                acc
            else
                let px = 1.-qx.[i]
                p (i-1) (px * (1. + acc))
        p ω 0.