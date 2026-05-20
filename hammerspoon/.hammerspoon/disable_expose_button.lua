-- Disables Mission Control / Exposé by consuming Ctrl+Up (keyCode 126).
-- A side button on the mouse synthesizes fn+Ctrl+Up, which is indistinguishable
-- from a real keyboard Ctrl+Up at the CGEventTap layer (same pid, stateID, etc.).
-- Rather than try to discriminate, we block the shortcut entirely.

local eventtap = require "hs.eventtap"
local event    = eventtap.event

local tap = eventtap.new({ event.types.keyDown }, function(e)
    if e:getKeyCode() ~= 126 then return false end
    local f = e:getFlags()
    if f.ctrl and not f.cmd and not f.alt and not f.shift then
        return true
    end
    return false
end)

tap:start()

return tap
