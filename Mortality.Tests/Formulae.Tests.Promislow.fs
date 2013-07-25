namespace Formulae.Tests.Promislow

// Tests derived from exercises in:
// Promislow, David (2011). Fundamentals of Actuarial Mathematics, 2nd Edn. Wiley, ISBN 978-0-470-68411-5

open System
open FsUnit
open NUnit.Framework
open Mortality

[<TestFixture>]
type ``Given Promislow exercises``() =

    let tolerance = 10E-16

    let qx = [|0.2; 0.25; 0.25; 0.30; 0.4; 0.0|]
    let lx = qx |> Formulae.qxToLx 1.

    let lx34 = Array.init 101 (fun x -> 100 - x)
               |> Array.map (float)

    let qx311 c1 c2 = 
        let ω = 119
        [|0..ω|] 
        |> Array.map (fun x -> if x < ω then
                                   1.-(Math.E ** (-c1 * (c2 ** float(x))))
                               else
                                   1.)

    [<Test>]
    member t.``The answer for exercise 3-1a is correct``() =
        let expected = [|1000.; 800.; 600.; 450.; 315.; 189.; 0.|]
        let actual = qx |> Formulae.qxToLx 1000.
        actual |> should equal expected
 
    [<Test>]
    member t.``The answer for exercise 3-1bi is correct``() =   
        let expected = 285./800.
        let actual = (Formulae.nqx lx 3 1) - (Formulae.nqx lx 1 1)                                     
        actual |> should (equalWithin tolerance) expected

    [<Test>]
    member t.``The answer for exercise 3-1b2 is correct``() =   
        let expected = 189./600.
        let actual = (Formulae.npx lx 3 2)                                   
        actual |> should (equalWithin tolerance) expected

    [<Test>]
    member t.``The answer for exercise 3-3 is correct``() =  
        let lx = [|100.; 90.; 75.; 55.; 0.;|]
        let expectedQ = [|1./10.; 1./6.; 4./15.; 1.|]
        let expectedP = 11./20.
        let qx = lx |> Formulae.lxToQx  
        let actualQ = qx
        let actualP = Formulae.npx lx 3 0                 
        actualQ |> should (equalWithin tolerance) expectedQ
        actualP |> should (equalWithin tolerance) expectedP

    [<TestCase(0, 0)>]
    [<TestCase(1, 2)>]
    [<TestCase(2, 4)>]
    [<TestCase(3, 6)>]
    member t.``The answer for exercise 3-4a is correct``(x, n) = 
        let expected = 1. - (float(n) / float(100 - x))
        let actual = Formulae.npx lx34 n x
        actual |> should (equalWithin tolerance) expected
     
    [<TestCase(0, 0)>]
    [<TestCase(1, 2)>]
    [<TestCase(2, 4)>]
    [<TestCase(3, 6)>]
    member t.``The answer for exercise 3-4b is correct``(x, n) = 
        let expected = (float(n) / float(100 - x))
        let actual = Formulae.nqx lx34 n x
        actual |> should (equalWithin tolerance) expected

    [<TestCase(0, 0, 0)>]
    [<TestCase(1, 2, 3)>]
    [<TestCase(2, 4, 6)>]
    [<TestCase(3, 6, 9)>]
    member t.``The answer for exercise 3-4c is correct``(x, n, k) =
        // The book has k / (100-x) but I believe this should be k / (100-x-n) 
        let expected = (float(k) / float(100 - x - n))
        let actual = Formulae.nqx lx34 k (x+n)
        actual |> should (equalWithin tolerance) expected

    [<TestCase(0.00005, 1.09, 79.83)>]
    [<TestCase(0.00006, 1.09, 77.72)>]
    [<TestCase(0.00005, 1.092, 78.41)>]
    member t.``The answers for exercise 3.8 are correct (using an lx table)``(c1, c2, expected) =
        // Answers in book only given to 2dp                                      
        let tolerance = 1E-2 

        let lx = qx311 c1 c2 |> Formulae.qxToLx 100000.
        let actual = Formulae.e lx 0
        actual |> should (equalWithin tolerance) expected

    [<TestCase(0.00005, 1.09, 79.83)>]
    [<TestCase(0.00006, 1.09, 77.72)>]
    [<TestCase(0.00005, 1.092, 78.41)>]
    member t.``The answers for exercise 3.8 are correct (using a qx table)``(c1, c2, expected) =
        // Answers in book only given to 2dp                                      
        let tolerance = 1E-2 

        let qx = qx311 c1 c2
        let actual = Formulae.eq qx 0
        actual |> should (equalWithin tolerance) expected
