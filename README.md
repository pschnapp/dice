# dice

This is a command-line dice calculator for use in D&D and such.

It is written in Haskell using Stack for package management.


## Usage

This calculator can do the standard arithmetic operations (besides division)
as well as rolling dice; it can roll dice normally, with advantage, or with
disadvantage.

The die-rolling operators are as follows:

  | Operator | Operation              |
  | -------: | ---------------------- |
  | `d`      | Roll normally          |
  | `>d`     | Roll with advantage    |
  | `<d`     | Roll with disadvantage |

These operators are used like this:

Say you wanted to roll for an attack with advantage and inspiration
(including an ability modifier and proficiency); you would type:

    {🎲}>  >d20 + d4 + 5
    total: 25
    breakdown:  1 >d 20 (Σ[(19>11)]=19) + 1 d 4 (Σ[1]=1) + 5

Then to roll for damage if the attack succeeds:

    {🎲}>  2d6 + 3
    total: 11
    breakdown:  2 d 6 (Σ[4,4]=8) + 3

Parentheses can be used to denote sub-expressions.


## Installation

First install Haskell Stack, e.g. on OSX with Homebrew:

    brew install haskell-stack

Next clone the repository and build and install the program:

    git clone https://github.com/pschnapp/dice.git
    cd dice
    stack install

Now you can run the calculator with:

    dice

Note: on Linux you might need to install `libtinfo-dev` as well to get it to work.


## Feature Wishlist

Some way to set a point total to roll for (e.g. when using hit-dice)
