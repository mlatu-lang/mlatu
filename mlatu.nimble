# Package

version       = "0.1.0"
author        = "Caden Haustein"
description   = "The best way forth"
license       = "CNPLv6+"
namedBin["src/main"] = "mlatu"
installDirs = @["assets"]

# Dependencies

requires "nim >= 1.4"
requires "sdl2"
