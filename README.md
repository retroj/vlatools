
# vlatools

This is a collection of utilities for working with the VLA file format.
VLA is a simple model format supported by the Digistar planetarium system.
The programs in this collection are licensed with either a BSD style
license or GPL -- refer to the list below for specifics.

Manuals for these tools may or may not be found in the 'doc/' directory of
the source code.  Have fun and may the source be with you.

## blender-vla (python, GPL)

A Blender plugin for importing and exporting VLA models.

## const3d (chicken-scheme, BSD)

Const3d generates a 3D model of a constellation boundary by projecting the
boundary out to several distances.  This allows visualization of a
constellation from non-Earth perspectives.  It supports output in VLA and
Wavefront OBJ.

## dspathtovla (python, BSD)

This program converts a Digistar CSV Path to a VLA.

## make-asterism (chicken-scheme, BSD)

A program for producing Digistar stick figure drawings.  Note: this
program does not yet produce VLA output, but another Digistar-specific
format used for asterism stick figures.  A VLA output option is intended
later.

## shpvla (php, BSD)

Shpvla converts ESRI Shapefiles to VLA.

## svgvla (chicken-scheme, BSD)

Svgvla is a tool for converting a small subset of SVG to VLA.

## vlaview (chicken-scheme, BSD)

Vlaview is a tool for viewing VLA models in an SDL display.
