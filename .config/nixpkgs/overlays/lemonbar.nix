self: super: {
  lemonbar =
    super.lemonbar-xft.overrideAttrs (selfAttrs: superAttrs: {
      patches = (superAttrs.patches or []) ++ [
        ./lemonbar-xft.patch
      ];
    });
}
