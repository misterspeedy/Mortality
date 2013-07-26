namespace Mortality

type InputType = Qx | Lx

type Table(inputType : InputType, baseAge : int, inputs : float[]) =

    let l0 = 100000. // By convention

    let _omega = baseAge + inputs.Length

    let _qx, _lx =
        match inputType with
        | Qx -> inputs, 
                inputs |> Formulae.qxToLx l0
        | Lx -> inputs |> Formulae.lxToQx,
                inputs

    member t.qx with get(index) = _qx.[index-baseAge]
    member t.lx with get(index) = _lx.[index-baseAge]
    member t.omega = _omega
