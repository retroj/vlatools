
## This file is part of blender-vla.
## Copyright (C) 2018  John J. Foerch
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

def make_chains (edges):
    chains = []
    for (a, b) in edges:
        for chain in chains:
            (cfirst, _) = chain[0]
            (_, clast) = chain[len(chain) - 1]
            if cfirst == a:
                chain.insert(0, (b, a))
                break
            elif cfirst == b:
                chain.insert(0, (a, b))
                break
            elif clast == a:
                chain.append((a, b))
                break
            elif clast == b:
                chain.append((b, a))
                break
        else:
            chains.append([(a, b)]) ## new chain
    return chains

def chains_to_vertex_chains (chains, vertices):
    vertex_chains = []
    for chain in chains:
        vertex_chain = [vertices[a].co for (a, b) in chain]
        last_vertex = chain[len(chain) - 1][1]
        vertex_chain.append(vertices[last_vertex].co)
        vertex_chains.append(vertex_chain)
    return vertex_chains

def write (filepath=False, units="meters", author="", site="",
           comment="", depthcue=0, **keywords):
    if units == "meters":
        coordsysleft = True
    else:
        coordsysleft = False
    f = open(filepath, "w")

    ## header
    ##
    f.write("set author {}\n".format(author))
    f.write("set site {}\n".format(site))
    f.write("set filetype NEW\n")
    f.write("set coordsys {}\n".format("LEFT" if coordsysleft else "RIGHT"))
    f.write("set defaultdraw STELLAR\n")
    f.write("set filecontent LINES\n") ##TODO: implement vertices option
    f.write("set parametric NON_PARAMETRIC\n") ##TODO: implement keyframe support
    f.write("set depthcue {}\n".format(depthcue))
    f.write("set library_id UNKNOWN\n")
    comments = comment.split("|")
    for c in comments:
        f.write("set comment {}\n".format(c))

    ## geometry
    ##
    chains = []
    for obj in bpy.context.selected_objects:
        vertices = dict(obj.data.vertices.items())
        edges = [tuple(edge.vertices) for (_, edge) in obj.data.edges.items()]
        chains.extend(chains_to_vertex_chains(make_chains(edges), vertices))
    for chain in chains:
        cmd = "P"
        for (x, y, z) in chain:
            f.write("{} {} {} {} 1.0\n".format(cmd, x, y, z))
            cmd = "L"
    f.close()
