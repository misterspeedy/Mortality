namespace Formulae.Generational.Tests.Promislow

// Tests derived from examples and exercises in:
// Promislow, David (2011). Fundamentals of Actuarial Mathematics, 2nd Edn. Wiley, ISBN 978-0-470-68411-5

open System
open FsUnit
open NUnit.Framework
open Mortality

[<TestFixture>]
type ``Given Promislow example 9-1``() =

    [<TestCase(2010, 0.9779)>]
    [<TestCase(1994, 0.9716)>]
    member t.``The answer is correct``(asAtYear, expected) =
        // Book only gives answers to 4dp:
        let tolerance = 50E-4 
        let qx = [for i in 0..59 -> 1.] @ [0.008576; 0.009633; 0.010911] |> Array.ofSeq
        let scale = [for i in 0..59 -> 0.] @ [0.016; 0.015; 0.015] |> Array.ofSeq
        let actual = Mortality.Formulae.Generational.npx qx scale 3 60 asAtYear 1994
        actual |> should (equalWithin tolerance) expected
