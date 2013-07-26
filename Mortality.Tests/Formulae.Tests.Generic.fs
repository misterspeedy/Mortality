namespace Formulae.Tests.Generic

open System
open FsUnit
open NUnit.Framework
open Mortality

[<TestFixture>]
type ``Given the mortality formulae and a generated lx table``() =

    let tolerance = 1E-11
    let w = 10
    let lx = [|0..w|] |> Array.map (fun x -> (100000-(x*10000)) |> float)
    let qx = lx |> Formulae.lxToQx

    // npx tests:

    [<Test>]
    member t.``the probability of living for 1 year at age 0 is 90000 / 100000``() =
        let expected = 90000. / 100000.
        let actual = Formulae.npx lx 1 0 
        actual |> should (equalWithin tolerance) expected

    [<Test>]
    member t.``the probability of living for 1 year at age w-2 is 10000. / 20000.``() =
        let expected = 10000. / 20000.
        let actual = Formulae.npx lx 1 (w-2) 
        actual |> should (equalWithin tolerance) expected

    [<Test>]
    member t.``the probability of living for 1 year at age w-1 is 0``() =
        let expected = 0.
        let actual = Formulae.npx lx 1 (w-1) 
        actual |> should (equalWithin tolerance) expected

    [<Test>]
    member t.``the probability of living for 2 years at age 0 is 80000 / 100000``() =
        let expected = 80000. / 100000.
        let actual = Formulae.npx lx 2 0 
        actual |> should (equalWithin tolerance) expected

    [<Test>]
    member t.``the probability of living for 2 years at age w-2 is 0``() =
        let expected = 0.
        let actual = Formulae.npx lx 2 (w-2)
        actual |> should (equalWithin tolerance) expected

    [<Test>]
    member t.``the probability of surviving over all ages is 0``() =
        let expected = 0
        let actual = Formulae.npx lx w 0
        actual |> should equal expected
        
    // nqx tests:

    [<Test>]
    member t.``the probability of dying plus the probability of living is 1. for each year``() =
        let expected = 1.
        [0..w]
        |> Seq.iter (fun x -> let live = Formulae.npx lx 1 x 
                              let die = Formulae.nqx lx 1 x
                              let actual = live + die
                              actual |> should (equalWithin tolerance) expected)

    [<Test>]
    member t.``the probability of dying over all ages is 1``() =
        let expected = 1
        let actual = Formulae.nqx lx w 0
        actual |> should equal expected

    // lxToQx, qxToLx tests:

    [<Test>]
    member t.``converting an lx table into a qx table and back again produces the same table``() =
        let expected = lx
        let actual = lx |> Formulae.lxToQx |> Formulae.qxToLx 100000.
        actual |> should (equalWithin tolerance) expected

    // e tests:

    [<Test>]
    member t.``the curtate life expectancy at age 0 is 4.5 years``() =
        let expected = 4.5
        let actual = Formulae.e lx 0
        actual |> should (equalWithin tolerance) expected

    [<Test>]
    member t.``the curtate life expectancy at age 5 is 2 years``() =
        let expected = 2
        let actual = Formulae.e lx 5
        actual |> should (equalWithin tolerance) expected

    [<Test>]
    member t.``the curtate life expectancy at age w-2 is 0.5 years``() =
        let expected = 0.5
        let actual = Formulae.e lx (w-2)
        actual |> should (equalWithin tolerance) expected

    [<Test>]
    member t.``the curtate life expectancy at age w-1 is 0 years``() =
        let expected = 0.
        let actual = Formulae.e lx (w-1)
        actual |> should (equalWithin tolerance) expected

    // eq tests:

    [<Test>]
    member t.``the curtate life expectancy at age 0 is 4.5 years, using qx table``() =
        let expected = 4.5
        let actual = Formulae.eq qx 0
        actual |> should (equalWithin tolerance) expected

    [<Test>]
    member t.``the curtate life expectancy at age 5 is 2 years, using qx table``() =
        let expected = 2
        let actual = Formulae.eq qx 5
        actual |> should (equalWithin tolerance) expected

    [<Test>]
    member t.``the curtate life expectancy at age w-2 is 0.5 years, using qx table``() =
        let expected = 0.5
        let actual = Formulae.eq qx (w-2)
        actual |> should (equalWithin tolerance) expected

    [<Test>]
    member t.``the curtate life expectancy at age w-1 is 0 years, using qx table``() =
        let expected = 0.
        let actual = Formulae.eq qx (w-1)
        actual |> should (equalWithin tolerance) expected

    [<Test>]
    member t.``the curtate life expectancy using an lx table is the same as when using a qx table for all ages``() =
        [0..w]
        |> Seq.iter (fun x -> let expected = Formulae.e lx x
                              let actual = Formulae.eq qx x
                              actual |> should (equalWithin tolerance) expected)

    // ec tests:

    [<Test>]
    member t.``Complete life expectancy is one half greater than curtate life expectancy, for all ages, using lx table``() =
        [0..w]
        |> Seq.iter (fun x -> let expected = (Formulae.e lx x) + 0.5
                              let actual = Formulae.ec lx x
                              actual |> should (equalWithin tolerance) expected)

    // ecq tests:

    [<Test>]
    member t.``Complete life expectancy is one half greater than curtate life expectancy, for all ages, using qx table``() =
        [0..w]
        |> Seq.iter (fun x -> let qx = lx |> Formulae.lxToQx 
                              let expected = (Formulae.eq qx x) + 0.5
                              let actual = Formulae.eqc qx x
                              actual |> should (equalWithin tolerance) expected)
