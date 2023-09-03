{ config, pkgs, ... }:

{
  home.username = "ratyaa";
  home.homeDirectory = "/home/ratyaa";

  home.packages = with pkgs; [
    # xorg.xorgserver
    # xorg.xf86inputevdev
    # xorg.xf86inputsynaptics
    # xorg.xf86inputlibinput
    # xorg.xf86videointel
    
    emacs
    lemonbar
    sxhkd
    bspwm
    picom
    feh
    fcitx5
    rofi-unwrapped
    st
    light
    redshift
    xdotool
    xdo
    telegram-desktop
    discord
    chromium
    stow
    fzf
    bc
    libnotify
    dunst
    tmux
    pinentry-gtk2
    pass
    nyxt
    qutebrowser
    (pkgs.linkFarm "dmenu" [ {
      name = "bin/dmenu";
      path = "${pkgs.rofi-unwrapped}/bin/rofi";
    } ])
  ];

  home.stateVersion = "23.05";
  programs.home-manager.enable = true;
}
