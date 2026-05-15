local M = {}

local dir = debug.getinfo(1, "S").source:sub(2):match("(.*/)")
local iconOn = hs.image.imageFromPath(dir .. "caffeine-on.pdf"):setSize({w=16, h=16})
local iconOff = hs.image.imageFromPath(dir .. "caffeine-off.pdf"):setSize({w=16, h=16})

local menubar = hs.menubar.new()
local awake = false

local function setState(state)
  awake = state
  hs.caffeinate.set("displayIdle", state, true)
  hs.caffeinate.set("systemIdle", state, true)
  hs.caffeinate.set("system", state, true)
  menubar:setIcon(state and iconOn or iconOff)
end

menubar:setClickCallback(function() setState(not awake) end)
setState(false)

return M
