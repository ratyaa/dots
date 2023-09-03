self: super: {
  emacs =
    (super.emacs29.override {
      withNativeCompilation = true;
      withXinput2 = false;
      withXwidgets = false;

      withGTK3 = true;
    }).overrideAttrs (selfAttrs: superAttrs: {
      configureFlags = (super.lib.lists.remove "--with-native-compilation" superAttrs.configureFlags)
                       ++ [
        "--without-xim"
        "--with-wide-int"
        "--with-native-compilation=aot"
      ];
    });
}
