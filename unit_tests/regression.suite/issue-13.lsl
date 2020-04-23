default
{
    state_entry()
    {
        llParticleSystem("" + llGetPhysicsMaterial());
        llParticleSystem((list)"" + llGetPhysicsMaterial());
        llParticleSystem(0 + llGetPhysicsMaterial());
        llParticleSystem((list)0 + llGetPhysicsMaterial());
    }
}
