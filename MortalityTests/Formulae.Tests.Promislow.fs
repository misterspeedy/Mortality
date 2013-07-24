module Mortality.Formulae.Tests.Promislow

// References:
//
//  Promislow, David, S. David (2011). Fundamentals of Actuarial Mathematics, 2nd Ed. ISBN 978-0-470-68411-5

open Mortality.Formulae
open NUnit.Framework
open FsUnit

[<TestFixture>]
type ``Given the mortality formulae and Promislow's exercises``() =
        
    let tolerance = 1E-10
    let lx = [|0..100|] |> Array.map (fun x -> (100 - x) |> float)
        
    // Promislow (2011) formula 3.11
    let qx311 c1 c2 = 
        [|0..119|]
        |> Array.map (fun x -> if x < 119 then
                                    1. - (System.Math.E ** (-(c1 * (c2 ** (x |> float)))))
                                else
                                    1.)

    // Promislow (2011) 3.9 Exercises, Exercise 3.4

    [<TestCase(1, 2, 3)>]
    [<TestCase(4, 5, 6)>]
    [<TestCase(7, 8, 9)>]
    [<TestCase(1, 2, 4)>]
    [<TestCase(4, 8, 16)>]
    [<TestCase(7, 14, 28)>]    
    member t.``the answer to exercise 3.4 part a is correct``(n, x, _) =
        let expected = 1. - (n |> float)/(100. - (x |> float))
        let actual = npx lx n x
        actual |> should (equalWithin tolerance) expected

    [<TestCase(1, 2, 3)>]
    [<TestCase(4, 5, 6)>]
    [<TestCase(7, 8, 9)>]
    [<TestCase(1, 2, 4)>]
    [<TestCase(4, 8, 16)>]
    [<TestCase(7, 14, 28)>]      
    member t.``the answer to exercise 3.4 part b is correct``(n, x, _) =
        let expected = (n |> float) / (100. - (x |> float))
        let actual = nqx lx n x
        actual |> should (equalWithin tolerance) expected

    [<TestCase(1, 2, 3)>] // 
    [<TestCase(4, 5, 6)>]
    [<TestCase(7, 8, 9)>]
    [<TestCase(1, 2, 4)>] //
    [<TestCase(4, 8, 16)>]
    [<TestCase(7, 14, 28)>]  
    member t.``the answer to exercise 3.4 part c is correct``(n, x, k) =
        // Promislow defines the answer as k/100-x which I believe is incorrect.  Needs to be k/100-x-n
        let expected = (k |> float) / ((100 - x - n) |> float)
        let actual = nqx lx (k) (x+n)
        actual |> should (equalWithin tolerance) expected

    // Promislow (2011) 3.9 Exercises, Exercise 3.6

    [<TestCase(1, 2)>]
    [<TestCase(4, 5)>]
    [<TestCase(7, 8)>]
    [<TestCase(1, 2)>]
    [<TestCase(4, 8)>]
    [<TestCase(7, 14)>]  
    member t.``the answer to exercise 3.6 is correct``(n, x) =
        let expected = 0.5 * (qx lx x) + ((px lx x) * (1. + (eo lx (x+1))))
        let actual = eo lx x
        actual |> should (equalWithin tolerance) expected

    // Promislow (2011) 3.9 Exercises, Exercise 3.8

    [<TestCase(0.00005, 1.09, 79.83)>]  
    [<TestCase(0.00006, 1.09, 77.72)>]  
    [<TestCase(0.00005, 1.092, 78.41)>]  
    member t.``the answer to exercise 3.8 is correct``(c1, c2, expected) =
        // Answers in book only given to 2dp:
        let tolerance = 0.005

        let lx311 = qx311 c1 c2 |> qxTolx
        let actual = e lx311 0
        actual |> should (equalWithin tolerance) expected

    [<TestCase(0.00005, 1.09, 79.83)>]  
    [<TestCase(0.00006, 1.09, 77.72)>]  
    [<TestCase(0.00005, 1.092, 78.41)>]  
    member t.``the answer to exercise 3.8 is correct (usinq eq version)``(c1, c2, expected) =
        // Answers in book only given to 2dp:
        let tolerance = 0.005

        let actual = eq (qx311 c1 c2) 0
        actual |> should (equalWithin tolerance) expected