local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.font = wezterm.font 'HackGen35 Console NF'
config.font_size = 12.0

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

config.window_background_opacity = 0.80
config.macos_window_background_blur = 5

function extract_filename(uri)
  local start, match_end = uri:find("$EDITOR:");
  if start == 1 then
    -- skip past the colon
    return uri:sub(match_end+1)
  end

  -- `file://hostname/path/to/file`
  local start, match_end = uri:find("file:");
  if start == 1 then
    -- skip "file://", -> `hostname/path/to/file`
    local host_and_path = uri:sub(match_end+3)
    local start, match_end = host_and_path:find("/")
    if start then
      -- -> `/path/to/file`
      return host_and_path:sub(match_end)
    end
  end

  return nil
end

wezterm.on("open-uri", function(window, pane, uri)
  local name = extract_filename(uri)
  if name then
    local editor = os.getenv("HOME") .. "/.dotfiles/bin/edit"

    -- To open a new window:
    window:perform_action(
      wezterm.action{SpawnCommandInNewWindow={
        args={editor, name}
      }}, 
      pane
    )

    -- prevent the default action from opening in a browser
    return false
  end
end)

config.hyperlink_rules = {
  -- These are the default rules, but you currently need to repeat
  -- them here when you define your own rules, as your rules override
  -- the defaults

  -- URL with a protocol
  {
    regex = "\\b\\w+://(?:[\\w.-]+)\\.[a-z]{2,15}\\S*\\b",
    format = "$0",
  },

  -- implicit mailto link
  {
      regex = "\\b\\w+@[\\w-]+(\\.[\\w-]+)+\\b",
      format = "mailto:$0",
  },

  -- new in nightly builds; automatically highly file:// URIs.
  {
      regex = "\\bfile://\\S*\\b",
      format = "$0"
  },

  -- Now add a new item at the bottom to match things that are
  -- probably filenames
  {
    regex = "(/|\\b)\\S*\\b",
    -- regex = "/Users/takezawa/dev/src/github.com/ttakezawa/dotfiles/.wezterm.lua",
    format = "$EDITOR:$0"
  },
}

return config
