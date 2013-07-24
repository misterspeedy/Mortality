namespace Mortality

module Formulae =

    /// Probability of still being alive between ages x and x + n
    let npx (lx : float[]) (n : int) (x : int) =
        let omega = lx.Length
        if x >= omega-1 then
            0.
        else
            lx.[x+n] / lx.[x]

    /// Probability of still being alive between ages x and x + 1
    let px (lx : float[]) x = 
        npx lx 1 x

    /// Probability of dying between ages x and x + n
    let nqx (lx : float[]) n x = 
        let omega = lx.Length
        if x >= omega-1 then
            1.
        else
            (lx.[x] - lx.[x+n]) / lx.[x]

    /// Probability of dying between ages x and x + 1
    let qx (lx : float[]) x = 
        nqx lx 1 x

    /// Convert an lx table to a qx table
    // TODO needs more tests
    let lxToqx lx =
        lx
        |> Array.mapi (fun x _ -> qx lx x)

    /// Convert a qx table to an lx table
    // TODO needs more tests
    let qxTolx (qx : float[]) = 
        let omega = qx.Length
        let lx = Array.zeroCreate omega
        lx.[0] <- 1.
        for x = 1 to omega-1 do
            let dx = lx.[x-1] * qx.[x-1]
            lx.[x] <- lx.[x-1] - dx
        lx

    /// Curtate life expectancy at age x - taking an lx table
    // TODO needs more tests
    let e (lx : float[]) x =
        let omega = lx.Length
        [1..(omega-x-1)]
        |> Seq.sumBy (fun k -> (lx.[x + k]) / lx.[x])

    /// Curtate life expectancy at age x - taking a qx table
    // TODO needs more tests
    let eq (qx : float[]) x =
        let omega = qx.Length
        let rec e x =
            if x >= omega-1 then 
                0.
            else
                (1.-qx.[x]) * (1.+e (x+1))
        e x
            
    /// Complete life expectancy at age x - taking an lx table
    let eo (lx : float[]) x  =
        0.5 + e lx x 

    /// Complete life expectancy at age x - taking a qx table
    let eoq (qx : float[]) x  =
        0.5 + eq qx x 

    /// Curtate n-year temporary life expectancy at age x
    // TODO needs tests
    let et (lx : float[]) n x =
        [1..n]
        |> Seq.sumBy (fun k -> npx lx n k)

    /// Curtate n-year temporary life expectancy at age x - taking a qx table
    /// TODO

    /// Complete n-year temporary life expectancy at age x
    /// TODO