namespace Mortality

module Formulae =

    /// Probability that someone of age x will survive to x+n, using an lx table
    let npx (lx : Table) n x =
        let w = lx.Length-1
        if x+n >= w+1 then
            0.
        else
            lx.[x+n] / lx.[x]

    /// Probability that someone of age x will die between ages x and x+n, using an lx table
    let nqx (lx : Table) n x =
        let w = lx.Length-1
        if x+n >= w+1 then
            1.
        else
            (lx.[x] - lx.[x+n]) / lx.[x]

    /// Convert an lx table into a qx table
    let lxToQx (lx : Table) =
        Array.init (lx.Length-1) (fun x -> nqx lx 1 x)

    /// Convert a qx table into an lx table, given an intial number of lives (conventionally, 100000)
    let qxToLx l0 (qx : Table) =
        let w = qx.Length+1
        let lx = Array.zeroCreate w
        lx.[0] <- l0
        for x in [1..w-2] do
            lx.[x] <- lx.[x-1] * (1.-qx.[x-1])
        lx.[w-1] <- 0.
        lx

    /// Curtate life expectancy at age x, using an lx table
    let e (lx : Table) x =
        let w = lx.Length-1
        [1..w-x-1]
        |> Seq.map (fun k -> npx lx k x)
        |> Seq.sum

    /// Curtate life expectancy at age x, using a qx table
    let eq (qx : Table) x =
        let w = qx.Length-1
        let rec p i acc =
            if i < x then
                acc
            else
                let px = 1.-qx.[i]
                p (i-1) (px * (1. + acc))
        p w 0.

    /// Complete life expectancy at age x, using an lx table
    let ec (lx : Table) x =
        (e lx x) + 0.5

    /// Complete life expectancy at age x, using a qx table
    let eqc (qx : Table) x =
        (eq qx x) + 0.5