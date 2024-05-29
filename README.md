# FigSynth

A polygonal synthesis program. Not particularly useful right now, but very fun
to use.

https://github.com/Swarthe/figsynth/assets/70660340/e8d1477c-e312-49f4-82d0-3e032e03db93

Inspiration and references:
- [Video](https://www.youtube.com/watch?v=2PuX7dqKJk0&t=18s) and
  [article](https://note.com/ysuie_o/n/n8ef016b59121) by Fendoap
- [White paper](https://quod.lib.umich.edu/cgi/p/pod/dod-idx/continuous-order-polygonalwaveform-synthesis.pdf?c=icmc;idno=bbp2372.2016.104;format=pdf)

## Installation

```sh
git clone https://github.com/Swarthe/figsynth
cd figsynth
cabal build
```

Dependencies:

- Cabal
- GHC
- OpenGL 3
- SDL 2
- GLEW 3

See [here](https://github.com/fjvallarino/monomer/blob/main/docs/tutorials/00-setup.md#libraries-sdl2-and-glew)
for more details.

### Usage

```sh
cabal run
```

Requires the SuperCollider server to be installed (`scsynth`).

This program has been tested on Arch Linux, and should generally work Unix/POSIX
systems.

## To do

- Improved performance and less allocation
- Higher resolution graph
- Pseudo 3D graph with mouse panning
- Ability to record, save and export sound samples

## License

Copyright (C) 2024 Emil Overbeck `<emil.a.overbeck at gmail dot com>`.

This file is part of `FigSynth`.

This software is free software: you can redistribute it and/or modify it under
the terms of version 3 of the GNU General Public License as published by the
Free Software Foundation.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <https://www.gnu.org/licenses/>.

### Fonts

This program includes [Roboto](https://fonts.google.com/specimen/Roboto),
licensed under the [Apache license](https://www.apache.org/licenses/LICENSE-2.0).
