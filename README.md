<img alt="HoneyTools" width=375 src=./res/logo.svg>

###### Honeycomb flakes, sheets, ribbons, whatever. Made easy.
-------
[![codacy](https://img.shields.io/codacy/grade/6cad511300d34773b5991a16933d7a7f?logo=codacy&style=flat-square)](https://www.codacy.com/gh/bellomia/HoneyTools/dashboard?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=bellomia/HoneyTools&amp;utm_campaign=Badge_Grade)
[![codecov](https://img.shields.io/codecov/c/github/bellomia/honeytools?label=coverage&logo=codecov&style=flat-square)](https://codecov.io/gh/bellomia/honeytools)


`HoneyTools` provides a set of Fortran modules to easily deal with nontrivial honeycomb structures in real-space: generate the coordinates, compute all the neighbor-shells, get direct access to logical masks for nearest and next-nearest neighbors (nth-order can be easily computed from the shell table), hence readily build tight-binding hamiltonians, or any other lattice quantity requiring real-space geometrical information.

- [Build system](#build-system)
- [Usage overview](#usage-overview)
- [Documentation](#documentation)
- [Gallery](#gallery)
- [License](#license)

## Build system

The library supports the Fortran Package Manager ([`fpm`](https://fpm.fortran-lang.org/en/index.html)). As such it can be built by simply invoking

```
fpm build
```
or directly installed in the system via

```
fpm install
```

Custom compiler and flags can be easily selected via environment variables (or command line options), as

| Environment Variable | Defines               | Overridden by  |
| :------------------- | :-------------------- | :------------- |
| `FPM_FC`             | Fortran compiler path | `--compiler`   |
| `FPM_CC`             | C compiler path       | `--c-compiler` |
| `FPM_FFLAGS`         | Fortran compiler flags| `--flag`       |
| `FPM_CFLAGS`         | C compiler flags      | `--c-flag`     |
| `FPM_AR`             | Archiver path         | `--archiver`   |
| `FPM_LDFLAGS`        | Link flags            | `--link-flag`  |

Finally the provided test programs can be ran with the special command

```
fpm test "test_name"
```
where the "test_name" string, referring to the source filenames (without `.f90` extension) contained in the `test/` directory, can be omitted to run the test-suite as a whole. This is regularly done in our dedicated CI workflow.

## Usage overview

The basic functionality is collected in the main, top-level, module, which you can include in your project as:
```fortran
use honeytools
```
It would often be accompanied by our _in-source_ plotting module, which provides ad hoc Fortran bindings to matplotlib and/or gnuplot, via the mighty [pyplot-fortran](https://github.com/jacobwilliams/pyplot-fortran) and [ogpf](https://github.com/kookma/ogpf) libraries. The basic use statement would be:
```fortran
use honeyplots, only: plot
```
Nevertheless we stress that the `honeytools` module provides all the core functionality to generate many common and not-so-common finite honeycomb structures: we have detached the plotting routines in a separated module to ensure a low dependency weight, especially in _non-sudo_ environments, where getting control on the exact version, path and configuration of `python`/`matplotlib` and `gnuplot` could easily become a hassle. 

A fairly comprehensive set of examples can be found in the project's wiki, if you are looking for an _unbelievably_ easy way to generate flakes, ribbons & stripes, islands... just [go there](https://github.com/bellomia/HoneyTools/wiki/A.-Basic-usage:-supercells,-stripes-and-flakes,-all-out-of-the-box!) and be happy.

For more advances usage you can instead include (a suitable subset of) the low-level modules, namely:

```fortran
use hex_coordinates
use hex_layout
use hex_geometries
use hex_neighbors
use xy_coordinates
use xy_neighbors
```

Which would provide access to the full API of the library. The usage is bounded to a good understanding of the core implementation: the special _cubic_ coordinates representing 2D hexagonal tessellations as a constrained 3D lattice. You can read extensively about the idea on this [delightful blog-post about videogame-development](https://www.redblobgames.com/grids/hexagons/), featuring fancy interactive visualizations and ---_for real_--- all you'll ever need to understand  in dept the underlying ideas. This package has taken profound inspiration from that article and the material it links to... not all that is described there is actually implemented here and not all you have here is explained there, but I could never provide a better coverage for the background theory. So yeah, take it a look!

The wiki provides an [advanced example](https://github.com/bellomia/HoneyTools/wiki/B.-An-advanced-example:-flake-with-random-vacancies-🚀) too, though you'll find it a rather small and probably incomplete account of what can be done: the library is so flexible that even a _huge hexagonal island with random vacancies_ can be coded in less than 100 lines of code (declarations included!).

## Documentation

The repository is configured for source-based, automated, API documentation via [FORD](https://github.com/Fortran-FOSS-Programmers/ford), so that you can generate the html files for your specific local version by running
```sh
ford docs-config.md
```
at the root directory of the project. Alternatively you can examine the officially generated docs for the latest git tag at `<insert-link>`.

## Gallery

Here some output from our `matplotlib` backend:

| Structure | Representation |
|:---------:|:--------------:|
|zigzag supercell|<img width=500 src=https://user-images.githubusercontent.com/56808633/191342648-1879fdb8-e98b-4365-bf5b-47148e8c6d9b.svg>|
|armchair stripe|<img width=500 src=https://user-images.githubusercontent.com/56808633/191429369-18b55155-3274-4548-aa13-5a38d36f8ea3.svg>|
|generic-radius flake|<img width=500 src=https://user-images.githubusercontent.com/56808633/191437811-0376f77f-db0e-4418-bf79-f7e6fa52979e.gif>|
|random-vacancy nanoflake|<img width=500 src=https://user-images.githubusercontent.com/56808633/191356110-696a0e32-01db-44be-95e2-b854465e2986.svg>|
|triangular islands|<img width=500 src=https://user-images.githubusercontent.com/56808633/191435668-a745faf6-7e94-41b9-b7df-3e87fddcaf2b.gif>|

## License

© Gabriele Bellomia, 2022

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License (LGPL) as published by the Free Software Foundation, either version 3 of the License, or any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU LGPL for more details.

You should have received a copy of the GNU LGPL along with this program. If not, see http://www.gnu.org/licenses/.
