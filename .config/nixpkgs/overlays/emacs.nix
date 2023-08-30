self: super: {
  emacs =
    (super.emacs29.override {
      withGTK3 = true;
      withNativeCompilation = false;
    }).overrideAttrs (selfAttrs: superAttrs: {
      configureFlags = superAttrs.configureFlags ++ [
        "--without-xim"
      ];
    });
}
