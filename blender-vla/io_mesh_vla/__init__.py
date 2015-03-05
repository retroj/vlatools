
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
    "version": (0,0,1),
    "location": "File > Import-Export",
    "description": "Import Digistar VLA models",
    "category": "Import-Export"
}

import bpy
from bpy.props import StringProperty, FloatProperty, BoolProperty, EnumProperty
import os

class VLAImporter (bpy.types.Operator):
    """Load Digistar VLA file data as a mesh"""
    bl_idname = "import_mesh.vla"
    bl_label = "Import VLA"
    bl_options = {"UNDO"}

    filepath = StringProperty(subtype = "FILE_PATH")
    filter_glob = StringProperty(default = "*.vla", options={"HIDDEN"})

    def execute (self, context):
        from . import import_vla
        import_vla.read(self.filepath)
        return {"FINISHED"}

    def invoke (self, context, event):
        wm = context.window_manager
        wm.fileselect_add(self)
        return {"RUNNING_MODAL"}


def menu_import (self, context):
    self.layout.operator(VLAImporter.bl_idname, text="Digistar VLA (.vla)")

def register ():
    bpy.utils.register_module(__name__)
    bpy.types.INFO_MT_file_import.append(menu_import)

def unregister ():
    bpy.utils.unregister_module(__name__)
    bpy.types.INFO_MT_file_import.remove(menu_import)

if __name__ == "__main__":
    register()
