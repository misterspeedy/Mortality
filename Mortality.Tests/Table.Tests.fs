namespace Table.Tests

open System
open FsUnit
open NUnit.Framework
open Mortality

[<TestFixture>]
type ``Given a Table instance``() =

    [<Test>]
    member t.``The table can be created from an array of q values and a base age``() =
        let actual = Table(Qx, 16, [|0.1; 0.2; 0.3|])
        actual |> should not' (be Null)

    [<Test>]
    member t.``The table can be created from an array of l values and a base age``() =
        let actual = Table(Qx, 16, [|3.;2.;1.|])
        actual |> should not' (be Null)

    [<Test>]
    member t.``The table has a qx property``() =
        let expected = typeof<float>
        let table = Table(Qx, 16, [|0.1; 0.2; 0.3|])
        let actual = (table.qx(16)).GetType()
        actual |> should equal expected

    [<Test>]
    member t.``The table has an lx property``() =
        let expected = typeof<float>
        let table = Table(Lx, 16, [|3.;2.;1.|])
        let actual = (table.qx(16)).GetType()
        actual |> should equal expected

    [<Test>]
    member t.``The table has an omega property``() =
        let expected = typeof<int>
        let table = Table(Qx, 16, [|0.1; 0.2; 0.3|])
        let actual = table.omega.GetType()
        actual |> should equal expected

    [<Test>]
    member t.``The omega property reflects the base age and the length of the input array``() =
        let expected = 19
        let table = Table(Qx, 16, [|0.1; 0.2; 0.3|])
        let actual = table.omega
        actual |> should equal expected

    [<Test>]
    member t.``Accessing the qx value at the base age reflects the first input value (when created from q values)``() =
        let expected = 0.1
        let table = Table(Qx, 16, [|0.1; 0.2; 0.3|])
        let actual = table.qx(16)
        actual |> should equal expected

    [<Test>]
    member t.``Accessing the lx value at the base age reflects the conventional l0 value of 100000 (when created from q values)``() =
        let expected = 100000
        let table = Table(Qx, 16, [|0.1; 0.2; 0.3|])
        let actual = table.lx(16)
        actual |> should equal expected

    [<Test>]
    member t.``Accessing the qx value at the base age reflects the first input value (when created from l values)``() =
        let expected = 0.5
        let table = Table(Lx, 16, [|100000.; 50000.; 40000.|])
        let actual = table.qx(16)
        actual |> should equal expected

    [<Test>]
    member t.``Accessing the lx value at the base age reflects the first input value (when created from l values)``() =
        let expected = 100000
        let table = Table(Qx, 16, [|0.1; 0.2; 0.3|])
        let actual = table.lx(16)
        actual |> should equal expected

    [<Test>]
    member t.``Accessing the qx value at age omega-1 reflects the last input value (when created from q values)``() =
        let expected = true
        let actual = false
        actual |> should equal expected

    [<Test>]
    member t.``Accessing the lx value at age omega-1 reflects the last input value (when created from q values)``() =
        let expected = true
        let actual = false
        actual |> should equal expected

    [<Test>]
    member t.``Accessing the qx value at age omega-1 reflects the last input value (when created from l values)``() =
        let expected = true
        let actual = false
        actual |> should equal expected

    [<Test>]
    member t.``Accessing the lx value at age omega-1 reflects the last input value (when created from l values)``() =
        let expected = true
        let actual = false
        actual |> should equal expected

    [<Test>]
    member t.``Accessing the qx value at age omega returns 1. (when created from l values)``() =
        let expected = true
        let actual = false
        actual |> should equal expected

    [<Test>]
    member t.``Accessing the lx value at age omega returns 0 (when created from l values)``() =
        let expected = true
        let actual = false
        actual |> should equal expected