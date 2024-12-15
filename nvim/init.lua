require("config.lazy")

require("config.ui")
-- order-dependent since "editing.lua" sets leader/local leader
require("config.editing")
require("config.navigation")
