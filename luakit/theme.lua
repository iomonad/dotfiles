--------------------------
-- Default luakit theme --
--------------------------

local theme = {}

-- Default settings
theme.font = "gmnterm"
theme.fg   = "#BFBFBF"
theme.bg   = "#131314"

-- Genaral colours
theme.success_fg ="#666680"
theme.loaded_fg  ="#CC7AB1"
theme.error_fg = "#BFBFBF"
theme.error_bg = "#407580"

-- Warning colours
theme.warning_fg = "#407580"
theme.warning_bg = "#BFBFBF"

-- Notification colours
theme.notif_fg = "#4C4C4C"
theme.notif_bg = "#BFBFBF"

-- Menu colours
theme.menu_fg                   = "#131314"
theme.menu_bg                   = "#BFBFBF"
theme.menu_selected_fg          = "#131314"
theme.menu_selected_bg          = "#407580"
theme.menu_title_bg             = "#BFBFBF"
theme.menu_primary_title_fg     = "#407580"
theme.menu_secondary_title_fg   = "#33333C"

-- Proxy manager
theme.proxy_active_menu_fg      = '#131314'
theme.proxy_active_menu_bg      = '#BFBFBF'
theme.proxy_inactive_menu_fg    = '#CCA3B1'
theme.proxy_inactive_menu_bg    = '#BFBFBF'

-- Statusbar specific
theme.sbar_fg         = "#407580"
theme.sbar_bg         = "#131314"

-- Downloadbar specific
theme.dbar_fg         = "#131314"
theme.dbar_bg         = "#407580"
theme.dbar_error_fg   = "#407580"

-- Input bar specific
theme.ibar_fg           = "#407580"
theme.ibar_bg           = "#131314"

-- Tab label
theme.tab_fg            = "#407580"
theme.tab_bg            = "#131314"
theme.tab_ntheme        = "#131314"
theme.selected_fg       = "#407580"
theme.selected_bg       = "#36364C"
theme.selected_ntheme   = "#36364C"
theme.loading_fg        = "#BACCAC"
theme.loading_bg        = "#131314"

-- Trusted/untrusted ssl colours
theme.trust_fg          = "#666680"
theme.notrust_fg        = "#407580"

return theme
-- vim: et:sw=4:ts=8:sts=4:tw=80
