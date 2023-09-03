self: super: {
  lemonbar =
    super.lemonbar-xft.overrideAttrs (selfAttrs: superAttrs: {
      patches = (superAttrs.patches or []) ++ [
        ./patches/bar/lemonbar-xft.patch
      ];
    });
}
