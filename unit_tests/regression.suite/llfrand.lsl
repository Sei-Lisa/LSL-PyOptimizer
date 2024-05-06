default { timer() {
    llParticleSystem(
         [ llFrand(-5)
         , llFrand(-5) > -5
         , llFrand(-5) <= 0
         , llFrand(-5) > 0
         , llFrand(-5.)
         , llFrand(-16777216)
         , llFrand(-16777216.)
         , llFrand(5)
         , llFrand(5) < 5
         , llFrand(5) >= 0
         , llFrand(5) < 0
         , llFrand(5.)
         , llFrand(16777216)
         , llFrand(16777216.)
         , llFrand(33554433)
         , llFrand(33554433.)
     ]);
}}
