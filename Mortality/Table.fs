namespace Mortality

// TODO: handle inputs with insufficient elements.

type InputType = Qx | Lx

type Table(inputType : InputType, baseAge : int, inputs : float[]) =

    let l0 = 100000. // By convention

    let _omega, _qx, _lx =
        match inputType with

        | Qx -> let inputs' = Array.append inputs [|1.|]
                baseAge + inputs.Length + 1,
                inputs', 
                inputs' |> Formulae.qxToLx l0

        | Lx -> let inputs' = Array.append inputs [|0.|]
                baseAge + inputs.Length,
                (inputs' |> Formulae.lxToQx),
                inputs'

    member t.qx with get(index) = _qx.[index-baseAge]
    member t.lx with get(index) = _lx.[index-baseAge]
    member t.omega = _omega
