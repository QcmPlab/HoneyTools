<img alt="HoneyTools" width=404 src=./res/logo.svg align="center">

###### Honeycomb flakes, sheets, ribbons, whatever. Made easy.
-------
[![build](https://img.shields.io/github/actions/workflow/status/qcmplab/honeytools/build.yaml?logo=Fortran&style=flat-square)](https://github.com/qcmplab/HoneyTools/actions/workflows/build.yaml)
[![test](https://img.shields.io/github/actions/workflow/status/qcmplab/honeytools/test.yaml?label=test-suite&logo=Fortran&style=flat-square)](https://github.com/qcmplab/HoneyTools/actions/workflows/test.yaml)
[![docs](https://img.shields.io/github/actions/workflow/status/qcmplab/honeytools/docs.yaml?label=docs-gen&logo=Fortran&style=flat-square)](https://qcmplab.github.io/HoneyTools/)
[![codecov](https://img.shields.io/codecov/c/github/qcmplab/honeytools?label=coverage&logo=codecov&style=flat-square)](https://codecov.io/gh/qcmplab/honeytools)
[![codacy](https://img.shields.io/codacy/grade/dc72f36f44b74dcda8b942c9d9760d4f?logo=codacy&style=flat-square)](https://app.codacy.com/gh/QcmPlab/HoneyTools/dashboard?branch=master)

`HoneyTools` provides a set of Fortran modules to easily deal with nontrivial honeycomb structures in real-space: generate the coordinates, compute all the neighbor-shells, get direct access to logical masks for nearest and next-nearest neighbors (nth-order can be easily computed from the shell table), hence readily build tight-binding hamiltonians, or any other lattice quantity requiring real-space geometrical information.

- [Installation as library](#installation-as-library)
- [Inclusion as `fpm` dependency](#inclusion-as-fpm-dependency)
- [Usage overview](#usage-overview)
- [Documentation](#documentation)
- [Gallery](#gallery)
- [License](#license)

## Installation as library

The library supports the Fortran Package Manager ([`fpm ≥ 0.9.0`](https://fpm.fortran-lang.org/en/index.html)). As such it can be built by simply invoking

```
fpm build
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

Then it can be install in the system via

```
fpm install
```
which would copy all the modules in the `$HOME/.local/include` directory and the library file in `$HOME/.local/lib`. This should suffice to manage linking with other libraries and programs. As an example you can write a [`pkg-config`](https://people.freedesktop.org/~dbn/pkg-config-guide.html) file as

```sh
# honeytools.pc
prefix=$HOME/.local
exec_prefix=${prefix}/bin
includedir=${prefix}/include
libdir=${prefix}/lib

Name: honeytools
Description: The HoneyTools library
Version: x.y.z
Cflags: -I${includedir}
Libs: -L${libdir} -lHoneyTools 
```
and then load it invoking 

```sh
export PKG_CONFIG_PATH=path/to/honeytools.pc:$PKG_CONFIG_PATH
```

In the case you may want to run locally the provided test programs. For that just type:

```
fpm test "test_name"
```
where the "test_name" string, referring to the source filenames (without `.f90` extension) contained in the `test/` directory, can be omitted to run the test-suite as a whole. This is regularly done in our dedicated CI workflow, anyway. 

Finally, if for any reason you suspect that `fpm` is messing with incremental compilation (or especially after installing the library, which moves away the modules...), you can reset the build by running:

```
fpm clean
```

## Inclusion as `fpm` dependency

If you want to add `HoneyTools` as a dependency for your own `fpm` project just add the following lines to your `fpm.toml` file:

```toml
[dependencies]
honeytools.git = "https://github.com/QcmPlab/HoneyTools.git"
honeytools.tag = "X.Y.Z" # choose a version among git tags
```

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

A fairly comprehensive set of examples can be found in the project's wiki, if you are looking for an _unbelievably_ easy way to generate flakes, ribbons & stripes, islands... just [go there](https://github.com/qcmplab/HoneyTools/wiki/A.-Basic-usage:-supercells,-stripes-and-flakes,-all-out-of-the-box!) and be happy.

For more advances usage you can instead include (a suitable subset of) the low-level modules, namely:

```fortran
use hex_coordinates
use hex_layout
use hex_geometries
use hex_neighbors
use xy_coordinates
use xy_neighbors
```

Which would provide access to the full API of the library. The usage is bound to a good understanding of the core implementation: the special _cubic_ coordinates representing 2D hexagonal tessellations as a constrained 3D lattice. You can read extensively about the idea on this [delightful blog-post about videogame-development](https://www.redblobgames.com/grids/hexagons/), featuring fancy interactive visualizations and ---_for real_--- all you'll ever need to understand  in dept the underlying ideas. This package has taken profound inspiration from that article and the material it links to... not all that is described there is actually implemented here and not all you have here is explained there, but I could never provide a better coverage for the background theory. So yeah, take it a look!

The wiki provides an [advanced example](https://github.com/qcmplab/HoneyTools/wiki/B.-An-advanced-example:-flake-with-random-vacancies-🚀) too, though you'll find it a rather small and probably incomplete account of what can be done: the library is so flexible that even a _huge hexagonal island with random vacancies_ can be coded in less than 100 lines of code (declarations included!).

## Documentation

The repository is configured for source-based, automated, API docs generation via [FORD](https://github.com/Fortran-FOSS-Programmers/ford), so that you can build the html files for your specific local version by running
```sh
ford docs.config
```
at the root directory of the project. Alternatively you can examine the [official docs for the latest commit on master](https://qcmplab.github.io/HoneyTools/), as generated by our continuous deployment workflow.

## Gallery

Here some output from our `matplotlib` backend:

| Structure | Representation |
|:---------:|:--------------:|
|zigzag supercell|<img width=500 src=https://raw.githubusercontent.com/wiki/QcmPlab/HoneyTools/gallery/supercell_zigzag.svg>|
|armchair stripe|<img width=500 src=https://raw.githubusercontent.com/wiki/QcmPlab/HoneyTools/gallery/stripe_armchair.svg>|
|generic-radius flake|<img width=500 src=https://raw.githubusercontent.com/wiki/QcmPlab/HoneyTools/gallery/flakes.gif>|
|random-vacancy nanoflake|<img width=500 src=https://raw.githubusercontent.com/wiki/QcmPlab/HoneyTools/gallery/holed2a.svg>|
|triangular islands|<img width=500 src=https://raw.githubusercontent.com/wiki/QcmPlab/HoneyTools/gallery/triangles.gif>|

## License

© Gabriele Bellomia, 2022

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License (LGPL) as published by the Free Software Foundation, either version 3 of the License, or any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU LGPL for more details.

You should have received a copy of the GNU LGPL along with this program. If not, see [gnu.org/licenses](http://www.gnu.org/licenses/).
