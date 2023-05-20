table.insert(alsa_monitor.rules, {
  matches = {
    {
      -- replace device as described below
      { "node.name", "equals", "device" },
    },
  },
  apply_properties = {
    -- If 64 doesn't work, try bigger values that are a multiple of 2 (128, 256, 512, 1024, 2048, etc.)
    ["api.alsa.headroom"] = 64,
  },
})
