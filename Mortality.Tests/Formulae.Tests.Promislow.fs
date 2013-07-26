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
        let w = 119
        [|0..w|] 
        |> Array.map (fun x -> if x < w then
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

    // Results from ch3.xls (downloaded from http://www.math.yorku.ca/Who/Faculty/Promislow/fundamentals_of_actuarial_mathematics/)
    [<TestCase(0,79.83084302)>]
    [<TestCase(1,78.83483466)>]
    [<TestCase(2,77.83913128)>]
    [<TestCase(3,76.84375545)>]
    [<TestCase(4,75.84873136)>]
    [<TestCase(5,74.85408488)>]
    [<TestCase(6,73.85984371)>]
    [<TestCase(7,72.86603749)>]
    [<TestCase(8,71.87269789)>]
    [<TestCase(9,70.87985879)>]
    [<TestCase(10,69.88755639)>]
    [<TestCase(11,68.89582934)>]
    [<TestCase(12,67.90471894)>]
    [<TestCase(13,66.91426927)>]
    [<TestCase(14,65.92452736)>]
    [<TestCase(15,64.93554337)>]
    [<TestCase(16,63.94737078)>]
    [<TestCase(17,62.96006657)>]
    [<TestCase(18,61.97369145)>]
    [<TestCase(19,60.98831004)>]
    [<TestCase(20,60.00399112)>]
    [<TestCase(21,59.02080783)>]
    [<TestCase(22,58.03883792)>]
    [<TestCase(23,57.05816401)>]
    [<TestCase(24,56.07887381)>]
    [<TestCase(25,55.10106043)>]
    [<TestCase(26,54.1248226)>]
    [<TestCase(27,53.15026496)>]
    [<TestCase(28,52.17749838)>]
    [<TestCase(29,51.20664019)>]
    [<TestCase(30,50.2378145)>]
    [<TestCase(31,49.27115251)>]
    [<TestCase(32,48.3067928)>]
    [<TestCase(33,47.3448816)>]
    [<TestCase(34,46.38557313)>]
    [<TestCase(35,45.42902988)>]
    [<TestCase(36,44.47542289)>]
    [<TestCase(37,43.52493206)>]
    [<TestCase(38,42.5777464)>]
    [<TestCase(39,41.63406433)>]
    [<TestCase(40,40.69409389)>]
    [<TestCase(41,39.75805299)>]
    [<TestCase(42,38.82616964)>]
    [<TestCase(43,37.89868211)>]
    [<TestCase(44,36.9758391)>]
    [<TestCase(45,36.05789984)>]
    [<TestCase(46,35.14513421)>]
    [<TestCase(47,34.23782278)>]
    [<TestCase(48,33.33625677)>]
    [<TestCase(49,32.44073804)>]
    [<TestCase(50,31.55157893)>]
    [<TestCase(51,30.66910212)>]
    [<TestCase(52,29.79364036)>]
    [<TestCase(53,28.92553618)>]
    [<TestCase(54,28.06514142)>]
    [<TestCase(55,27.21281685)>]
    [<TestCase(56,26.3689315)>]
    [<TestCase(57,25.53386204)>]
    [<TestCase(58,24.70799202)>]
    [<TestCase(59,23.89171098)>]
    [<TestCase(60,23.08541349)>]
    [<TestCase(61,22.28949806)>]
    [<TestCase(62,21.50436598)>]
    [<TestCase(63,20.73042)>]
    [<TestCase(64,19.96806295)>]
    [<TestCase(65,19.21769622)>]
    [<TestCase(66,18.47971818)>]
    [<TestCase(67,17.75452246)>]
    [<TestCase(68,17.04249618)>]
    [<TestCase(69,16.3440181)>]
    [<TestCase(70,15.65945668)>]
    [<TestCase(71,14.98916811)>]
    [<TestCase(72,14.33349427)>]
    [<TestCase(73,13.69276074)>]
    [<TestCase(74,13.06727471)>]
    [<TestCase(75,12.45732298)>]
    [<TestCase(76,11.86316996)>]
    [<TestCase(77,11.28505574)>]
    [<TestCase(78,10.72319426)>]
    [<TestCase(79,10.17777155)>]
    [<TestCase(80,9.648944144)>]
    [<TestCase(81,9.13683762)>]
    [<TestCase(82,8.641545314)>]
    [<TestCase(83,8.163127261)>]
    [<TestCase(84,7.701609333)>]
    [<TestCase(85,7.256982622)>]
    [<TestCase(86,6.829203081)>]
    [<TestCase(87,6.418191417)>]
    [<TestCase(88,6.023833274)>]
    [<TestCase(89,5.645979685)>]
    [<TestCase(90,5.284447802)>]
    [<TestCase(91,4.939021915)>]
    [<TestCase(92,4.609454722)>]
    [<TestCase(93,4.295468872)>]
    [<TestCase(94,3.996758735)>]
    [<TestCase(95,3.712992398)>]
    [<TestCase(96,3.443813859)>]
    [<TestCase(97,3.188845386)>]
    [<TestCase(98,2.947690015)>]
    [<TestCase(99,2.719934157)>]
    [<TestCase(100,2.505150287)>]
    [<TestCase(101,2.302899659)>]
    [<TestCase(102,2.112735039)>]
    [<TestCase(103,1.934203403)>]
    [<TestCase(104,1.76684857)>]
    [<TestCase(105,1.610213727)>]
    [<TestCase(106,1.463843798)>]
    [<TestCase(107,1.327287581)>]
    [<TestCase(108,1.200099508)>]
    [<TestCase(109,1.08184076)>]
    [<TestCase(110,0.972079016)>]
    [<TestCase(111,0.870385073)>]
    [<TestCase(112,0.776321388)>]
    [<TestCase(113,0.6894081)>]
    [<TestCase(114,0.609021229)>]
    [<TestCase(115,0.534071174)>]
    [<TestCase(116,0.461912546)>]
    [<TestCase(117,0.384334415)>]
    [<TestCase(118,0.271428219)>]
    [<TestCase(119,0.)>]
    // Two tests in one to avoid repeating many test cases.
    member t.``The curtate life expectancy is the same as given in ch3.xls (using an lx or a qx table)``(x, expected) =
        let tolerance = 1E-8

        let c1, c2 = 0.00005, 1.09

        let qx = qx311 c1 c2
        let lx = qx |> Formulae.qxToLx 100000.

        let actualL = Formulae.e lx x
        let actualQ = Formulae.eq qx x

        actualL |> should (equalWithin tolerance) expected
        actualQ |> should (equalWithin tolerance) expected

