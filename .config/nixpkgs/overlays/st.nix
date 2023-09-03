self: super: {
  st = super.st.overrideAttrs (selfAttrs: superAttrs: rec {
    patches = [
      ./patches/st/alpha.patch
      ./patches/st/scrollback.patch
      # ./patches/st/scrollback-reflow.patch
      ./patches/st/scrollback-mouse.patch
    ];
    configFile = super.writeText "config.h"
        (builtins.readFile ~/.config/st/config.h);
    postPatch = [ superAttrs.postPatch ] ++ [ ''cp ${selfAttrs.configFile} config.def.h'' ];
  });				
}
