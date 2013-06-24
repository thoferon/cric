# To-Do

## Error-handling

If the SSH connection fails or something, it could create a new session and retry the last action.

Maybe 3 tries before crashing.

## Logging

Simple support for colours when stdout = tty. (stdoutLogger and debugLogger)

## Files

Versioning?

## Package managers

Like `apt-get update`. What about the others ?

Support for other package manager (aptitude, emerge, pacman, FreeBSD's port tree.

Support to install a package from a file. (then specifying the package manager)

## Services

Function to send a signal to a daemon (start, stop, restart, ...)

Autodetection of the system used (initd, upstart, ...)

## Users and groups

Creation, removal of groups. Adding a user to a group, ...
