# Linenoise Line Editing and History

Both interactive clients (`interactive_client.exe` and `interactive_client_v2.exe`) now support line editing and command history using the linenoise library.

## Features

### Command History
- **Up/Down arrows**: Navigate through command history
- **Ctrl-R**: Reverse search through history
- **Persistent history**: Commands are saved to `~/.hdl_history` and persist between sessions
- **1000 commands**: History limited to last 1000 commands

### Line Editing
- **Left/Right arrows**: Move cursor within current line
- **Home/End**: Jump to beginning/end of line
- **Ctrl-A/Ctrl-E**: Same as Home/End
- **Ctrl-K**: Delete from cursor to end of line
- **Ctrl-U**: Delete from cursor to beginning of line
- **Ctrl-W**: Delete previous word
- **Backspace/Delete**: Delete characters
- **Ctrl-L**: Clear screen

### Multi-line Support
- **Multi-line mode enabled**: Allows editing longer Lua expressions
- Line wrapping for commands that exceed terminal width

### Exit
- **Ctrl-D**: Exit the interactive client
- **Ctrl-C**: Cancel current line (returns to prompt)

## Usage

Simply start the interactive client:

```bash
./_build/default/interactive_client_v2.exe
```

The history is automatically loaded from `~/.hdl_history` on startup and saved on exit.

## Example Session

```
hdl> dump.stats('sysver_tests/slib_clock_div.vhd')
[... output ...]

hdl> optimize.quick('sysver_tests/slib_clock_div.vhd')
[... output ...]

# Press Up arrow twice to get back to dump.stats command
hdl> dump.stats('sysver_tests/slib_clock_div.vhd')
[... output ...]

# Use arrow keys to edit the previous command
hdl> dump.stats('sysver_tests/slib_input_sync.vhd')
[... output ...]
```

## History File Location

Commands are saved to:
```
~/.hdl_history
```

You can view/edit this file directly if needed:
```bash
cat ~/.hdl_history
tail -20 ~/.hdl_history  # View last 20 commands
```

## Benefits Over Basic readline

Linenoise provides:
- **Lightweight**: No external dependencies (pure C library with OCaml bindings)
- **Cross-platform**: Works on Linux, macOS, and Windows
- **No configuration needed**: Works out of the box
- **Minimal overhead**: Fast and efficient

## Technical Details

### Implementation

Both clients use the `linenoise` OCaml library:

```ocaml
(* Configure linenoise *)
let history_file = Filename.concat (Sys.getenv "HOME") ".hdl_history" in
let _ = LNoise.history_set ~max_length:1000 in
let _ = LNoise.history_load ~filename:history_file in
LNoise.set_multiline true;

(* REPL loop *)
let rec loop () =
    match LNoise.linenoise "hdl> " with
    | None -> (* Ctrl-D - exit *)
    | Some line ->
        if String.trim line <> "" then
            ignore (LNoise.history_add line);
        (* ... evaluate line ... *)
        loop ()
```

### Dependencies

Added to `dune` file:
```
(libraries ... linenoise)
```

Installed via opam:
```bash
opam install linenoise
```

## Comparison: Before vs After

### Before (basic readline)
```ocaml
let line = read_line () in
(* No history, no line editing, no arrow key support *)
```

**Limitations:**
- No command history
- No arrow key navigation
- No line editing (must retype errors)
- No history persistence

### After (linenoise)
```ocaml
match LNoise.linenoise "hdl> " with
| Some line -> (* ... *)
```

**Benefits:**
- Full command history (Up/Down arrows)
- Line editing (Left/Right arrows, Home/End, etc.)
- Persistent history across sessions
- Multi-line support
- Screen clearing (Ctrl-L)

## Future Enhancements

Potential additions:
- **Tab completion**: Auto-complete module names, file paths, commands
- **Syntax hints**: Show function signatures as you type
- **Colored hints**: Highlight syntax errors before execution
- **Custom keybindings**: Configure keyboard shortcuts

These can be added using linenoise's callback functions:
```ocaml
LNoise.set_completion_callback (fun line completions ->
    (* Add completions based on current line *)
)

LNoise.set_hints_callback (fun line ->
    (* Return hints based on current line *)
)
```
