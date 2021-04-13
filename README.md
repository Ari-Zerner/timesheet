# Timesheet
A command-line interface for tracking hours worked.

# Disclaimer
Timesheet is primarily intended as a personal project for me to practice Haskell. If it's useful to you, so much the better, but I make no guarantees about its functionality.

Currently, Timesheet doesn't implement persistence, and should be considered a prototype.

# Installation
The recommended way to install and run Timesheet is to clone this repository and use `stack run` (https://docs.haskellstack.org/en/stable/README/).

# Usage
Timesheet features a contextual command list, such that different commands are available depending on program state. At any time, the `?` command can be used to display the currently available commands.

The core concepts of Timesheet are shifts and periods. A shift is the block of time between clocking in and clocking out. A period is a group of consecutive shifts.