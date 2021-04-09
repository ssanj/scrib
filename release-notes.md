# 3.0.8

## Bug Fixes

### New Note button was disable on the Save page.
- The `New Note` was disabled on the `Save` and there was not way to create a new note. The user had to click on the main menu to make this happen.

### Delete button was enable on the `Save` page when editing an unsaved note.
- The `Delete` button was incorrectly enable when it shouldn't have been - when a note was not saved remote and hence shouldn't be deletable.

# 3.0.7

## Features

### Delete note functionality on the `Save` screen.
- Deletes a saved note, remotely and from the session cache
- Already deleted notes are removed from the UI without error
- Failures on Notes that can't be deleted
- Errors on trying to save a deleted note

### Notes can be ~struck through~.
- Supports a `[deleted]` tag which enables strike through titles

## Bug Fixes
- Saving a note on the `Save` page and then going to the `Index` page and back resulted in the `Save` page showing the previously saved note. The `Save` page is now initialized correctly.
