========
Raindrop
========

.. image:: https://travis-ci.org/lancelet/raindrop.svg?branch=master
    :target: https://travis-ci.org/lancelet/raindrop
.. image:: https://coveralls.io/repos/github/lancelet/raindrop/badge.svg?branch=master
    :target: https://coveralls.io/github/lancelet/raindrop?branch=master


Raindrop is a 2D renderer. It uses a signed-area scanline rasterization
algorithm, similar to the algorithm used by `Pathfinder 3`_.

.. _Pathfinder 3: https://github.com/servo/pathfinder

Approximate TODO list
=====================

- [X] Tile-based alpha stencil rendering.
- [ ] Converting paths to tiles.
- [ ] Tile filling.
- [ ] Stroking paths.

Background reading
==================

The following articles provide useful background documentation:

- `stb_truetype v2`_ describes the signed-area alpha stencil rendering algorithm
  that is generally credited to Raph Levien in the `libart` library.
- `A look at Pathfinder`_ describes the higher-level tiling that Pathfinder uses
  to identify fully-filled tiles and re-caps the signed-area approach.

.. _stb_truetype v2: https://nothings.org/gamedev/rasterize/
.. _A look at Pathfinder: https://nical.github.io/posts/a-look-at-pathfinder.html
