local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.font = wezterm.font 'HackGen35 Console NF'
config.font_size = 13.0

config.use_ime = true

config.default_cursor_style = 'BlinkingBar'
config.enable_scroll_bar = true
config.cursor_blink_rate = 400
config.cursor_blink_ease_in = 'Constant'
config.cursor_blink_ease_out = 'Constant'

config.window_decorations = "RESIZE"
config.show_new_tab_button_in_tab_bar = false
config.window_frame = {
  inactive_titlebar_bg = "none",
  active_titlebar_bg = "none",
}
config.window_background_gradient = {
  colors = { "#000000" },
}

return config
