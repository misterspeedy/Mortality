module Mortality.Formulae.Tests.Generic

open Mortality.Formulae
open NUnit.Framework
open FsUnit

[<TestFixture>]
type ``Given the mortality formulae and an artificial lx table``() =

    let tolerance = 1E-10
    let omega = 100
    let lx = [|0..omega-1|] |> Array.map (fun x -> (1. - ((x |> float)/100.)) |> float)

    [<Test>]
    member t.``The probability of dying over all ages is 1``() =
        let expected = 1
        let actual = nqx lx 0 omega
        actual |> should (equalWithin tolerance) expected

    [<Test>]
    member t.``The probability of surviving over all ages is 0``() =
        let expected = 0
        let actual = npx lx 0 omega
        actual |> should (equalWithin tolerance) expected

    [<Test>]
    member t.``For every age, px1 == 1 - qx1``() =
        for x in [0..omega] do
            let expected = 1. - qx lx 1
            let actual = px lx 1
            actual |> should (equalWithin tolerance) expected

    [<Test>]
    member t.``For every age and a range of periods, pxn == 1 - qxn``() =
        for x in [0..omega] do
            for n in [0..9] do
                let expected = 1. - qx lx n
                let actual = px lx n
                actual |> should (equalWithin tolerance) expected

    [<Test>]
    member t.``Converting an lx table to a qx table and back produces the orginal table``() =
        let expected = lx
        let actual = lx |> lxToqx |> qxTolx
        actual |> should equal expected

    [<Test>]
    member t.``The curtate life expectancy using an lx table matches the curtate life expectancy using a qx table``() =
        let qx = lx |> lxToqx
        for x in [0..omega] do
            let expected = eq qx x
            let actual = e lx x
            actual |> should (equalWithin tolerance) expected

    [<Test>]
    member t.``The complete life expectancy using an lx table is 1/2 greater than the curtate life expectancy``() =
        for x in [0..omega] do
            let expected = (e lx x) + 0.5
            let actual = eo lx x
            actual |> should (equalWithin tolerance) expected

    [<Test>]
    member t.``The complete life expectancy using a qx table is 1/2 greater than the curtate life expectancy``() =
        let qx = lx |> lxToqx
        for x in [0..omega] do
            let expected = (eq qx x) + 0.5
            let actual = eoq qx x
            actual |> should (equalWithin tolerance) expected