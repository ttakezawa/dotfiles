local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.window_decorations = "RESIZE"
config.show_new_tab_button_in_tab_bar = false
config.window_frame = {
  inactive_titlebar_bg = "none",
  active_titlebar_bg = "none",
}
config.window_background_gradient = {
  colors = { "#000000" },
}

config.font = wezterm.font 'HackGen35 Console NF'
config.font_size = 12.0

config.window_background_opacity = 0.80
config.macos_window_background_blur = 5



return config
