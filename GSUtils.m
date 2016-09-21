(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: GSUtils *)
(* :Context: GSUtils` *)
(* :Author: Beskrovnykh Andrey *)
(* :Date: 2016-09-05 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) Beskrovnykh Andrey (beskrovnykh) andrew173139@gmail.com *)
(* :Keywords: InterpolatingFunction, Plot, Plot3D *)
(* :Discussion: *)

BeginPackage["GSUtils`"];

maxBounds::usage = "maxBounds[fun] or maxBounds[fun[t, x]]
calculates domain max bounds tmax and xmax of InterpolatingFunctions 'fun'";

plot::usage = "plot[funs, params] show 3d-plot of InterpolatedFunctions
'funs' of 2 variables t and x";

plotAtT::usage = "shows 2d-plot of InterpolatedFunctions
'funs' of 2 variables t and x at 'tVal' time as a function of x. 'funs'
is a list of functions of two variables t, x, 'legend' is a list of string";

plotAtX::usage = "plotAtX[funs, legend, xVal, params] shows 2d-plot of InterpolatedFunctions
'funs' of 2 variables t and x at the point 'xVal' as a function of time. 'funs'
is a list of functions of two variables t, x, 'legend' is a list of string";

Begin["`Private`"]

maxBounds[ifun_?(Head[#]===InterpolatingFunction&)] :=
    ifun["Domain"] /. {min_?NumberQ, max_?NumberQ} -> max;

maxBounds[ifun_] :=
    Head[ifun]["Domain"] /. {min_?NumberQ, max_?NumberQ} -> max;

plot[funs__, params___] :=
    Block[{T, X},
    {T, X} = maxBounds[First[funs]];
    Plot3D[Evaluate[funs /. Symbol["x"] -> x /. Symbol["t"] -> t], {t, 0, T}, {x, 0, X},
      AxesLabel -> {"time", "x", ""}, params]
];

plotAtX[funs__, legend___, xVal_] :=
    Module[{T, X}, {T, X} = maxBounds[First[funs]];
    Plot[Evaluate[funs /. Symbol["x"] -> xVal /. Symbol["t"] -> t], {t, 0, T},
      PlotLegends -> Placed[legend, Above],
      AxesLabel -> {"time"}]
    ];

plotAtT[funs__, legend___, tVal_] :=
    Module[{T, X}, {T, X} = maxBounds[First[funs]];
    Plot[Evaluate[funs /. Symbol["x"] -> x /. Symbol["t"] -> tVal], {x, 0, X},
      PlotLegends -> Placed[legend, Above],
      AxesLabel -> {"x"}]
    ];

End[];

EndPackage[];