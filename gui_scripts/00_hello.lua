-- 00_hello.lua — smoke test that gui.* bindings reach the menubar.
-- Loaded automatically by sv_gui at startup.

gui.add_item("Scripts", "Hello world", "demo_hello")

function demo_hello()
  gui.message("Hello from Lua!\nlablgtk3 + lua-ml are wired up.")
  gui.set_status("Said hello")
end
