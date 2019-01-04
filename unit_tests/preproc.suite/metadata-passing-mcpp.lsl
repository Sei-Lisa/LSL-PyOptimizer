#define VERBATIM_STRINGIFY(x) #x
#define STRINGIFY(x) VERBATIM_STRINGIFY(x)
default{state_entry(){
  __AGENTID__;
  __AGENTKEY__;
  STRINGIFY(__AGENTIDRAW__);
  __LINE__;
  __SHORTFILE__;
  __AGENTNAME__;
  STRINGIFY(__ASSETID__);
}}
