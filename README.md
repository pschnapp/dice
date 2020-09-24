# dice

This is a command-line dice calculator for use with D&D and such.

It is written in Haskell using Stack for package management.


## Installation

On OSX, install Homebrew, clone the repository, run:

    brew install haskell-stack
    stack run

WIP

## Usage

Upon running you will be greeted with a prompt at which you can roll dice:

    {🎲}> 2d6 + 8 + 4d4
    total: 27
    breakdown:  2d6 (2, 2)  +  8  +  4d4 (3, 4, 4, 4)
    {🎲}> 

You can roll any of d4, d6, d8, d10, d12, d20 or d100, add numeric literals to your rolls, or multiple numeric literals together:

    {🎲}> 6d8 + 3 * 7
    total: 54
    breakdown:  6d8 (3, 2, 6, 8, 7, 7)  +  3 * 7
