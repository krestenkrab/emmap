# Erlang MMAP `emmap`

This Erlang library provides a wrapper that allows you to memory map files into the Erlang memory space.  


## Basic Usage

The basic usage is

    {ok, Mem} = emmap:open("filename", [read, shared, direct]),
    {ok, Binary} = file:pread(Mem, 100, 40),
    ...
    ok = file:close(Mem).

The open options is a list containing zero or more of these:

- `read`, `write`: Open for reading and/or writing (you can specify both).
- `private`, `shared`: The file is opened with copy-on-write semantics, or sharing memory with the underlying file.
- `direct`: read/pread operations do not copy memory, but rather use "resource binaries" that can change content if the underlying data is changed.  This is the most performant, but also has other implications.
- `lock`, `nolock` do (or do not) use a semaphore to control state changes internally in the NIF library.  
- `auto_unlink` automatically deletes the mapped file after the mapped data was garbage collected. This can be used when the mapped file is a file-based shared-memory area (e.g. `/run/shm/...`) and is mapped in `direct` mode to free the memory after the data was gc'd.

From this point, `Mem` can be used with the `file` operations

- `{ok, Binary} = file:pread(Mem, Position, Length)` read Length bytes at Position in the file.
- `ok = file:pwrite(Mem, Position, Binary)` writes to the given position. 
- `{ok, Binary} = file:read(Mem, Length)` read 1..Length bytes from current position, or return `eof` if pointer is at end of file.
- `{ok, Pos} = file:position(Mem, Where)` see file:position/2 documentation.
- `ok = file:close(Mem)`

## Notes

Using the option `direct` has the effect that the mmap file is not closed until all references to binaries coming out of read/pread have been garbage collected.  This is a consequence of that such binaries are referring directly to the mmap'ed memory.  

