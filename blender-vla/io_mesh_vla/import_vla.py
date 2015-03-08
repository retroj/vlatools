
## This file is part of blender-vla.
## Copyright (C) 2015  John J. Foerch
##
## blender-vla is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## blender-vla is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with blender-vla.  If not, see <http://www.gnu.org/licenses/>.

import bpy
import operator

def addprop (props, spec):
    field = spec[1].upper()
    if field == 'COORDSYS':
        props[field] = spec[2].upper() ## LEFT (m) or RIGHT (ly)
    return props

def readvla (filepath, name,
             divide_ly=False,
             respect_coordsys=True,
             **keywords):
    props = {'COORDSYS': 'LEFT'}
    f = open(filepath, "r")
    verts = {}
    verti = 0
    edges = []
    p = False
    def divide_out_lightyears (x):
        return x / 9.4607304725808e15
    def apply_optional_transformations (v):
        if props['COORDSYS'] == 'LEFT':
            if divide_ly:
                v = map(divide_out_lightyears, v)
            if respect_coordsys:
                [x, y, z] = v
                v = [x, z, y]
        return v
    for line in f.readlines():
        line_split = line.split()
        if len(line_split) == 0:
            continue
        kw = line_split[0].upper()
        if kw == 'SET':
            props = addprop(props, line_split[1:])
        if kw == 'P':
            v = map(float, line_split[1:4])
            v = tuple(apply_optional_transformations(v))
            if v not in verts:
                verts[v] = verti
                verti += 1
            p = verts[v]
        elif kw == 'L':
            v = map(float, line_split[1:4])
            v = tuple(apply_optional_transformations(v))
            if v not in verts:
                verts[v] = verti
                verti += 1
            l = verts[v]
            edges.append([p, l])
            p = l
    f.close()

    ## sort verts by index, and convert the keys to lists
    verts = sorted(verts.items(), key=operator.itemgetter(1))
    verts = [list(k) for k, v in verts]
    mesh = bpy.data.meshes.new(name)
    mesh.from_pydata(verts, edges, [])
    return mesh


def addmesh (mesh, name):
    scene = bpy.context.scene
    for o in scene.objects:
        o.select = False
    mesh.update()
    mesh.validate()
    nobj = bpy.data.objects.new(name, mesh)
    scene.objects.link(nobj)
    nobj.select = True
    if scene.objects.active is None or scene.objects.active.mode == 'OBJECT':
        scene.objects.active = nobj


def read (filepath=False, **keywords):
    name = bpy.path.display_name_from_filepath(filepath)
    mesh = readvla(filepath, name, **keywords)
    addmesh(mesh, name)
