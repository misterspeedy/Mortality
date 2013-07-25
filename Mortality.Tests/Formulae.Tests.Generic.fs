namespace Formulae.Tests.Generic

open System
open FsUnit
open NUnit.Framework
open Mortality

// TODO: Need many more generic tests 
//
// Eg. qx == 1-px for each age
//     Some actual numbers
//     Empty array handling
//     Bad parameter handling

[<TestFixture>]
type ``Given the mortality formulae and a generated lx table``() =

    let ω = 10
    let lx = [|0..ω|] |> Array.map (fun x -> (100000-(x*10000)) |> float)

    [<Test>]
    member t.``the probability of surviving over all ages is 0``() =
        let expected = 0
        let actual = Formulae.npx lx ω 0
        actual |> should equal expected
        
    [<Test>]
    member t.``the probability of dying over all ages is 1``() =
        let expected = 1
        let actual = Formulae.nqx lx ω 0
        actual |> should equal expected

    [<Test>]
    member t.``converting an lx table into a qx table and back again produces the same table``() =
        let tolerance = 1E-11
        let expected = lx
        let actual = lx |> Formulae.lxToQx |> Formulae.qxToLx 100000.
        actual |> should (equalWithin tolerance) expected
