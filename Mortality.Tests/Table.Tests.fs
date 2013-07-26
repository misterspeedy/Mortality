namespace Table.Tests

open System
open FsUnit
open NUnit.Framework
open Mortality

[<TestFixture>]
type ``Given a Table instance``() =

    [<Test>]
    member t.``The table can be created from an array of q values and a base age``() =
        let expected = true
        let actual = false
        actual |> should equal expected

    [<Test>]
    member t.``The table can be created from an array of l values and a base age``() =
        let expected = true
        let actual = false
        actual |> should equal expected

    [<Test>]
    member t.``The table has a qx property``() =
        let expected = true
        let actual = false
        actual |> should equal expected

    [<Test>]
    member t.``The table has an lx property``() =
        let expected = true
        let actual = false
        actual |> should equal expected

    [<Test>]
    member t.``The table has an omega property``() =
        let expected = true
        let actual = false
        actual |> should equal expected

    [<Test>]
    member t.``The omega property reflects the base age and the length of the input array``() =
        let expected = true
        let actual = false
        actual |> should equal expected

    [<Test>]
    member t.``Accessing the qx value at the base age reflects the first input value (when created from q values)``() =
        let expected = true
        let actual = false
        actual |> should equal expected

    [<Test>]
    member t.``Accessing the lx value at the base age reflects the first input value (when created from q values)``() =
        let expected = true
        let actual = false
        actual |> should equal expected

    [<Test>]
    member t.``Accessing the qx value at the base age reflects the first input value (when created from l values)``() =
        let expected = true
        let actual = false
        actual |> should equal expected

    [<Test>]
    member t.``Accessing the lx value at the base age reflects the first input value (when created from l values)``() =
        let expected = true
        let actual = false
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