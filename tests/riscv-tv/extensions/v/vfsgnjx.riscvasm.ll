; vfsgnjx flips the sign of vs2 where vs1 is negative, keeping NaN payloads
define <2 x double> @test(<2 x double> %x, <2 x double> %y) {
  %xi = bitcast <2 x double> %x to <2 x i64>
  %yi = bitcast <2 x double> %y to <2 x i64>
  %s = and <2 x i64> %yi, splat (i64 -9223372036854775808)
  %r = xor <2 x i64> %xi, %s
  %rf = bitcast <2 x i64> %r to <2 x double>
  ret <2 x double> %rf
}
