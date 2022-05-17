// Test cases based on bug report by SaladDais@users.noreply.github.com;
// see https://github.com/Sei-Lisa/LSL-PyOptimizer/issues/17
// The first two cases are a change of behaviour in Mono, which used to return
// zero when a string that represented a denormal was converted to float (LSO
// did not have this problem).

// The last two cases are a screwup on the author's side.
[ (string)((float)"-0.0")
, (string)(1.4e-45 == (float)"1.4e-45")
, (string)0.00000000000001
, (string)-0.00000000000001
]
