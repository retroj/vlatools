
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

bl_info = {
    "name": "Digistar VLA format",
    "author": "John J Foerch <jjfoerch@earthlink.net>",
    "blender": (2,6,9),
    "version": (1,0,0),
    "location": "File > Import-Export",
    "description": "Import and Export Digistar VLA models",
    "category": "Import-Export"
}

import bpy
from bpy.props import *
import os

def export_load_options (self, context):
    if not os.path.exists(self.header):
        return None
    props = {}
    f = open(self.header, "r")
    for line in f.readlines():
        line_split = line.split(None, 2)
        if len(line_split) == 0:
            continue
        kw = line_split[0].upper()
        if kw == "SET":
            key = line_split[1].upper()
            val = line_split[2].strip()
            if key == "COMMENT" and key in props:
                ##XXX: it would be nicer to have an array of strings for
                ##     comments, but I'm not sure how to do that in the
                ##     blender plugin api.
                props[k] = props[k] + "|" + val
            else:
                props[key] = val
        else:
            break
    f.close()
    if "FILECONTENT" in props:
        self.subtype = props["FILECONTENT"].lower()
    if "COORDSYS" in props:
        if props["COORDSYS"].upper() == "RIGHT":
            self.units = "ly"
        else:
            self.units = "meters"
    if "DEPTHCUE" in props:
        self.depthcue = int(props["DEPTHCUE"])
    if "AUTHOR" in props:
        self.author = props["AUTHOR"]
    if "SITE" in props:
        self.site = props["SITE"]
    if "COMMENT" in props:
        self.comment = props["COMMENT"]
    return None

class VLAExporter (bpy.types.Operator):
    """Save mesh as Digistar VLA file"""
    bl_idname = "export_mesh.vla"
    bl_label = "Export VLA"
    bl_options = {"UNDO"}

    filepath = StringProperty(subtype = "FILE_PATH")
    filter_glob = StringProperty(default = "*.vla", options={"HIDDEN"})

    subtype = EnumProperty(
        items = [("dots", "Dots", "Export vertices"),
                 ("lines", "Lines", "Export edges")],
        name = "Subtype",
        description = "VLA subtype (dots or lines)",
        default = "lines")
    units = EnumProperty(
        items = [("meters", "Meters", "Meters (implies COORDSYS LEFT)"),
                 ("ly", "Light Years", "Light Years (implies COORDSYS RIGHT)")],
        name = "Units",
        description = "Units of coordinates in VLA",
        default = "meters")
    author = StringProperty(name = "Author")
    site = StringProperty(name = "Site")
    comment = StringProperty(name = "Comment")
    depthcue = IntProperty(name = "Depthcue", min=0, max=2)

    header = StringProperty(name = "Header",
                            description = "Load export options from VLA file...",
                            update = export_load_options)

    def execute (self, context):
        from . import export_vla
        keywords = self.as_keywords()
        export_vla.write(**keywords)
        return {"FINISHED"}

    def invoke (self, context, event):
        wm = context.window_manager
        wm.fileselect_add(self)
        return {"RUNNING_MODAL"}


class VLAImporter (bpy.types.Operator):
    """Load Digistar VLA file data as a mesh"""
    bl_idname = "import_mesh.vla"
    bl_label = "Import VLA"
    bl_options = {"UNDO"}

    filepath = StringProperty(subtype = "FILE_PATH")
    filter_glob = StringProperty(default = "*.vla", options={"HIDDEN"})

    divide_ly = BoolProperty(
        name="Divide Ly",
        description="Divide out lightyears for very large models in meters",
        default=False)

    respect_coordsys = BoolProperty(
        name="Respect Coordsys",
        description="Transform left-handed coordinates to right-handed coordinates",
        default=True)

    def execute (self, context):
        from . import import_vla
        keywords = self.as_keywords()
        import_vla.read(**keywords)
        return {"FINISHED"}

    def invoke (self, context, event):
        wm = context.window_manager
        wm.fileselect_add(self)
        return {"RUNNING_MODAL"}


def menu_export (self, context):
    self.layout.operator(VLAExporter.bl_idname, text="Digistar VLA (.vla)")

def menu_import (self, context):
    self.layout.operator(VLAImporter.bl_idname, text="Digistar VLA (.vla)")

def register ():
    bpy.utils.register_module(__name__)
    bpy.types.INFO_MT_file_export.append(menu_export)
    bpy.types.INFO_MT_file_import.append(menu_import)

def unregister ():
    bpy.utils.unregister_module(__name__)
    bpy.types.INFO_MT_file_export.remove(menu_export)
    bpy.types.INFO_MT_file_import.remove(menu_import)

if __name__ == "__main__":
    register()
